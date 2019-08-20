------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . L O A D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Par;
with Restrict; use Restrict;
with Scn;      use Scn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Sinput.L; use Sinput.L;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uname;    use Uname;

package body Lib.Load is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function From_Limited_With_Chain return Boolean;
   --  Check whether a possible circular dependence includes units that
   --  have been loaded through limited_with clauses, in which case there
   --  is no real circularity.

   function Spec_Is_Irrelevant
     (Spec_Unit : Unit_Number_Type;
      Body_Unit : Unit_Number_Type) return Boolean;
   --  The Spec_Unit and Body_Unit parameters are the unit numbers of the
   --  spec file that corresponds to the main unit which is a body. This
   --  function determines if the spec file is irrelevant and will be
   --  overridden by the body as described in RM 10.1.4(4). See description
   --  in "Special Handling of Subprogram Bodies" for further details.

   procedure Write_Dependency_Chain;
   --  This procedure is used to generate error message info lines that
   --  trace the current dependency chain when a load error occurs.

   ------------------------------
   -- Change_Main_Unit_To_Spec --
   ------------------------------

   procedure Change_Main_Unit_To_Spec is
      U : Unit_Record renames Units.Table (Main_Unit);
      N : File_Name_Type;
      X : Source_File_Index;

   begin
      --  Get name of unit body

      Get_Name_String (U.Unit_File_Name);

      --  Note: for the following we should really generalize and consult the
      --  file name pattern data, but for now we just deal with the common
      --  naming cases, which is probably good enough in practice ???

      --  Change .adb to .ads

      if Name_Len >= 5
        and then Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb"
      then
         Name_Buffer (Name_Len) := 's';

      --  Change .2.ada to .1.ada (Rational convention)

      elsif Name_Len >= 7
        and then Name_Buffer (Name_Len - 5 .. Name_Len) = ".2.ada"
      then
         Name_Buffer (Name_Len - 4) := '1';

      --  Change .ada to _.ada (DEC convention)

      elsif Name_Len >= 5
        and then Name_Buffer (Name_Len - 3 .. Name_Len) = ".ada"
      then
         Name_Buffer (Name_Len - 3 .. Name_Len + 1) := "_.ada";
         Name_Len := Name_Len + 1;

      --  No match, don't make the change

      else
         return;
      end if;

      --  Try loading the spec

      N := Name_Find;
      X := Load_Source_File (N);

      --  No change if we did not find the spec

      if X <= No_Source_File then
         return;
      end if;

      --  Otherwise modify Main_Unit entry to point to spec

      U.Unit_File_Name := N;
      U.Source_Index := X;
   end Change_Main_Unit_To_Spec;

   -------------------------------
   -- Create_Dummy_Package_Unit --
   -------------------------------

   function Create_Dummy_Package_Unit
     (With_Node : Node_Id;
      Spec_Name : Unit_Name_Type) return Unit_Number_Type
   is
      Unum         : Unit_Number_Type;
      Cunit_Entity : Entity_Id;
      Cunit        : Node_Id;
      Du_Name      : Node_Or_Entity_Id;
      End_Lab      : Node_Id;
      Fname        : constant File_Name_Type :=
        Get_File_Name (Spec_Name, Subunit => False);
      Pre_Name : constant Boolean :=
        Is_Predefined_File_Name (Fname, Renamings_Included => False);
      Ren_Name : constant Boolean :=
        Is_Predefined_Renaming_File_Name (Fname);
      GNAT_Name : constant Boolean :=
        Is_GNAT_File_Name (Fname);
      Save_CS : constant Boolean := Get_Comes_From_Source_Default;

   begin
      --  The created dummy package unit does not come from source

      Set_Comes_From_Source_Default (False);

      --  Normal package

      if Nkind (Name (With_Node)) = N_Identifier then
         Cunit_Entity :=
           Make_Defining_Identifier (No_Location,
             Chars => Chars (Name (With_Node)));
         Du_Name := Cunit_Entity;
         End_Lab := New_Occurrence_Of (Cunit_Entity, No_Location);

      --  Child package

      else
         Cunit_Entity :=
           Make_Defining_Identifier (No_Location,
             Chars => Chars (Selector_Name (Name (With_Node))));
         Du_Name :=
           Make_Defining_Program_Unit_Name (No_Location,
             Name => Copy_Separate_Tree (Prefix (Name (With_Node))),
             Defining_Identifier => Cunit_Entity);

         Set_Is_Child_Unit (Cunit_Entity);

         End_Lab :=
           Make_Designator (No_Location,
             Name => Copy_Separate_Tree (Prefix (Name (With_Node))),
             Identifier => New_Occurrence_Of (Cunit_Entity, No_Location));
      end if;

      Set_Scope (Cunit_Entity, Standard_Standard);

      Cunit :=
        Make_Compilation_Unit (No_Location,
          Context_Items => Empty_List,
          Unit =>
            Make_Package_Declaration (No_Location,
              Specification =>
                Make_Package_Specification (No_Location,
                  Defining_Unit_Name   => Du_Name,
                  Visible_Declarations => Empty_List,
                  End_Label            => End_Lab)),
          Aux_Decls_Node =>
            Make_Compilation_Unit_Aux (No_Location));

      --  Mark the dummy package as analyzed to prevent analysis of this
      --  (non-existent) unit in -gnatQ mode because at the moment the
      --  structure and attributes of this dummy package does not allow
      --  a normal analysis of this unit

      Set_Analyzed (Cunit);

      Units.Increment_Last;
      Unum := Units.Last;

      Units.Table (Unum) :=
        (Cunit                  => Cunit,
         Cunit_Entity           => Cunit_Entity,
         Dependency_Num         => 0,
         Dynamic_Elab           => False,
         Error_Location         => Sloc (With_Node),
         Expected_Unit          => Spec_Name,
         Fatal_Error            => Error_Detected,
         Generate_Code          => False,
         Has_RACW               => False,
         Filler                 => False,
         Ident_String           => Empty,

         Is_Predefined_Renaming => Ren_Name,
         Is_Predefined_Unit     => Pre_Name or Ren_Name,
         Is_Internal_Unit       => Pre_Name or Ren_Name or GNAT_Name,
         Filler2                => False,

         Loading                => False,
         Main_Priority          => Default_Main_Priority,
         Main_CPU               => Default_Main_CPU,
         Primary_Stack_Count    => 0,
         Sec_Stack_Count        => 0,
         Munit_Index            => 0,
         No_Elab_Code_All       => False,
         Serial_Number          => 0,
         Source_Index           => No_Source_File,
         Unit_File_Name         => Fname,
         Unit_Name              => Spec_Name,
         Version                => 0,
         OA_Setting             => 'O');

      Init_Unit_Name (Unum, Spec_Name);

      Set_Comes_From_Source_Default (Save_CS);
      Set_Error_Posted (Cunit_Entity);
      Set_Error_Posted (Cunit);
      return Unum;
   end Create_Dummy_Package_Unit;

   -----------------------------
   -- From_Limited_With_Chain --
   -----------------------------

   function From_Limited_With_Chain return Boolean is
      Curr_Num : constant Unit_Number_Type :=
                   Load_Stack.Table (Load_Stack.Last).Unit_Number;

   begin
      --  True if the current load operation is through a limited_with clause
      --  and we are not within a loop of regular with_clauses.

      for U in reverse Load_Stack.First .. Load_Stack.Last - 1 loop
         if Load_Stack.Table (U).Unit_Number = Curr_Num then
            return False;

         elsif Present (Load_Stack.Table (U).With_Node)
           and then Limited_Present (Load_Stack.Table (U).With_Node)
         then
            return True;
         end if;
      end loop;

      return False;
   end From_Limited_With_Chain;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Units.Init;
      Load_Stack.Init;
   end Initialize;

   ------------------------
   -- Initialize_Version --
   ------------------------

   procedure Initialize_Version (U : Unit_Number_Type) is
   begin
      Units.Table (U).Version := Source_Checksum (Source_Index (U));
   end Initialize_Version;

   ----------------------
   -- Load_Main_Source --
   ----------------------

   procedure Load_Main_Source is
      Fname : constant File_Name_Type := Next_Main_Source;
      Pre_Name : constant Boolean :=
        Is_Predefined_File_Name (Fname, Renamings_Included => False);
      Ren_Name : constant Boolean :=
        Is_Predefined_Renaming_File_Name (Fname);
      GNAT_Name : constant Boolean :=
        Is_GNAT_File_Name (Fname);
      Version : Word := 0;

   begin
      Load_Stack.Increment_Last;
      Load_Stack.Table (Load_Stack.Last) := (Main_Unit, Empty);

      --  Initialize unit table entry for Main_Unit. Note that we don't know
      --  the unit name yet, that gets filled in when the parser parses the
      --  main unit, at which time a check is made that it matches the main
      --  file name, and then the Unit_Name field is set. The Cunit and
      --  Cunit_Entity fields also get filled in later by the parser.

      Units.Increment_Last;

      Units.Table (Main_Unit).Unit_File_Name := Fname;

      if Fname /= No_File then
         Main_Source_File := Load_Source_File (Fname);
         Current_Error_Source_File := Main_Source_File;

         if Main_Source_File > No_Source_File then
            Version := Source_Checksum (Main_Source_File);

         else
            --  To avoid emitting a source location (since there is no file),
            --  we write a custom error message instead of using the machinery
            --  in errout.adb.

            Set_Standard_Error;

            if Main_Source_File = No_Access_To_Source_File then
               Write_Str
                 ("no read access for file """ & Get_Name_String (Fname)
                  & """");
            else
               Write_Str
                 ("file """ & Get_Name_String (Fname) & """ not found");
            end if;

            Write_Eol;
            Set_Standard_Output;
         end if;

         Units.Table (Main_Unit) :=
           (Cunit                  => Empty,
            Cunit_Entity           => Empty,
            Dependency_Num         => 0,
            Dynamic_Elab           => False,
            Error_Location         => No_Location,
            Expected_Unit          => No_Unit_Name,
            Fatal_Error            => None,
            Generate_Code          => False,
            Has_RACW               => False,
            Filler                 => False,
            Ident_String           => Empty,

            Is_Predefined_Renaming => Ren_Name,
            Is_Predefined_Unit     => Pre_Name or Ren_Name,
            Is_Internal_Unit       => Pre_Name or Ren_Name or GNAT_Name,
            Filler2                => False,

            Loading                => True,
            Main_Priority          => Default_Main_Priority,
            Main_CPU               => Default_Main_CPU,
            Primary_Stack_Count    => 0,
            Sec_Stack_Count        => 0,

            Munit_Index            => 0,
            No_Elab_Code_All       => False,
            Serial_Number          => 0,
            Source_Index           => Main_Source_File,
            Unit_File_Name         => Fname,
            Unit_Name              => No_Unit_Name,
            Version                => Version,
            OA_Setting             => 'O');
      end if;
   end Load_Main_Source;

   ---------------
   -- Load_Unit --
   ---------------

   function Load_Unit
     (Load_Name         : Unit_Name_Type;
      Required          : Boolean;
      Error_Node        : Node_Id;
      Subunit           : Boolean;
      Corr_Body         : Unit_Number_Type := No_Unit;
      Renamings         : Boolean          := False;
      With_Node         : Node_Id          := Empty;
      PMES              : Boolean          := False) return Unit_Number_Type
   is
      Calling_Unit : Unit_Number_Type;
      Uname_Actual : Unit_Name_Type;
      Unum         : Unit_Number_Type;
      Unump        : Unit_Number_Type;
      Fname        : File_Name_Type;
      Pre_Name     : Boolean;
      Ren_Name     : Boolean;
      GNAT_Name    : Boolean;
      Src_Ind      : Source_File_Index;
      Save_PMES    : constant Boolean := Parsing_Main_Extended_Source;

      Save_Cunit_Restrictions : constant Save_Cunit_Boolean_Restrictions :=
                                  Cunit_Boolean_Restrictions_Save;
      --  Save current restrictions for restore at end

   begin
      Parsing_Main_Extended_Source := PMES;

      --  Initialize restrictions to config restrictions for unit to load if
      --  it is part of the main extended source, otherwise reset them.

      --  Note: it's a bit odd but PMES is False for subunits, which is why
      --  we have the OR here. Should be investigated some time???

      if PMES or Subunit then
         Restore_Config_Cunit_Boolean_Restrictions;
      else
         Reset_Cunit_Boolean_Restrictions;
      end if;

      --  If renamings are allowed and we have a child unit name, then we
      --  must first load the parent to deal with finding the real name.
      --  Retain the with_clause that names the child, so that if it is
      --  limited, the parent is loaded under the same condition.

      if Renamings and then Is_Child_Name (Load_Name) then
         Unump :=
           Load_Unit
             (Load_Name  => Get_Parent_Spec_Name (Load_Name),
              Required   => Required,
              Subunit    => False,
              Renamings  => True,
              Error_Node => Error_Node,
              With_Node  => With_Node);

         if Unump = No_Unit then
            Parsing_Main_Extended_Source := Save_PMES;
            return No_Unit;
         end if;

         --  If parent is a renaming, then we use the renamed package as
         --  the actual parent for the subsequent load operation.

         if Nkind (Unit (Cunit (Unump))) = N_Package_Renaming_Declaration then
            Uname_Actual :=
              New_Child
                (Load_Name, Get_Unit_Name (Name (Unit (Cunit (Unump)))));

            --  If the load is for a with_clause, for visibility purposes both
            --  the renamed entity and renaming one must be available in the
            --  current unit: the renamed one in order to retrieve the child
            --  unit, and the original one because it may be used as a prefix
            --  in the body of the current unit. We add an explicit with_clause
            --  for the original parent so that the renaming declaration is
            --  properly loaded and analyzed.

            if Present (With_Node) then
               Insert_After (With_Node,
                 Make_With_Clause (Sloc (With_Node),
                   Name => Copy_Separate_Tree (Prefix (Name (With_Node)))));
            end if;

            --  Save the renaming entity, to establish its visibility when
            --  installing the context. The implicit with is on this entity,
            --  not on the package it renames. This is somewhat redundant given
            --  the with_clause just created, but it simplifies subsequent
            --  expansion of the current with_clause. Optimizable ???

            if Nkind (Error_Node) = N_With_Clause
              and then Nkind (Name (Error_Node)) = N_Selected_Component
            then
               declare
                  Par : Node_Id := Name (Error_Node);

               begin
                  while Nkind (Par) = N_Selected_Component
                    and then Chars (Selector_Name (Par)) /=
                             Chars (Cunit_Entity (Unump))
                  loop
                     Par := Prefix (Par);
                  end loop;

                  --  Case of some intermediate parent is a renaming

                  if Nkind (Par) = N_Selected_Component then
                     Set_Entity (Selector_Name (Par), Cunit_Entity (Unump));

                  --  Case where the ultimate parent is a renaming

                  else
                     Set_Entity (Par, Cunit_Entity (Unump));
                  end if;
               end;
            end if;

         --  If the parent is not a renaming, then get its name (this may
         --  be different from the parent spec name obtained above because
         --  of renamings higher up in the hierarchy).

         else
            Uname_Actual := New_Child (Load_Name, Unit_Name (Unump));
         end if;

      --  Here if unit to be loaded is not a child unit

      else
         Uname_Actual := Load_Name;
      end if;

      Fname     := Get_File_Name (Uname_Actual, Subunit);
      Pre_Name  :=
        Is_Predefined_File_Name (Fname, Renamings_Included => False);
      Ren_Name  := Is_Predefined_Renaming_File_Name (Fname);
      GNAT_Name := Is_GNAT_File_Name (Fname);

      if Debug_Flag_L then
         Write_Eol;
         Write_Str ("*** Load request for unit: ");
         Write_Unit_Name (Load_Name);

         if Required then
            Write_Str (" (Required = True)");
         else
            Write_Str (" (Required = False)");
         end if;

         Write_Eol;

         if Uname_Actual /= Load_Name then
            Write_Str ("*** Actual unit loaded: ");
            Write_Unit_Name (Uname_Actual);
         end if;
      end if;

      --  Capture error location if it is for the main unit. The idea is to
      --  post errors on the main unit location, not the most recent unit.
      --  Note: Unit_Name (Main_Unit) is not set if we are parsing gnat.adc.

      if Present (Error_Node)
        and then Unit_Name (Main_Unit) /= No_Unit_Name
      then
         --  It seems like In_Extended_Main_Source_Unit (Error_Node) would
         --  do the trick here, but that's wrong, it is much too early to
         --  call this routine. We are still in the parser, and the required
         --  semantic information is not established yet. So we base the
         --  judgment on unit names.

         Get_External_Unit_Name_String (Unit_Name (Main_Unit));

         declare
            Main_Unit_Name : constant String := Name_Buffer (1 .. Name_Len);

         begin
            Get_External_Unit_Name_String
              (Unit_Name (Get_Source_Unit (Error_Node)));

            --  If the two names are identical, then for sure we are part
            --  of the extended main unit

            if Main_Unit_Name = Name_Buffer (1 .. Name_Len) then
               Load_Msg_Sloc := Sloc (Error_Node);

            --  If the load is called from a with_type clause, the error
            --  node is correct.

            --  Otherwise, check for the subunit case, and if so, consider
            --  we have a match if one name is a prefix of the other name.

            else
               if Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
                    or else
                  Nkind (Unit (Cunit (Get_Source_Unit (Error_Node)))) =
                                                                N_Subunit
               then
                  Name_Len := Integer'Min (Name_Len, Main_Unit_Name'Length);

                  if Name_Buffer (1 .. Name_Len)
                        =
                     Main_Unit_Name (1 .. Name_Len)
                  then
                     Load_Msg_Sloc := Sloc (Error_Node);
                  end if;
               end if;
            end if;
         end;
      end if;

      --  If we are generating error messages, then capture calling unit

      if Present (Error_Node) then
         Calling_Unit := Get_Source_Unit (Error_Node);
      else
         Calling_Unit := No_Unit;
      end if;

      --  See if we already have an entry for this unit

      Unum := Unit_Names.Get (Uname_Actual);
      if Unum = No_Unit then
         Unum := Units.Last + 1;
      end if;

      --  Whether or not the entry was found, Unum is now the right value,
      --  since it is one more than Units.Last (i.e. the index of the new
      --  entry we will create) in the not found case.

      --  A special check is necessary in the unit not found case. If the unit
      --  is not found, but the file in which it lives has already been loaded,
      --  then we have the problem that the file does not contain the unit that
      --  is needed. We simply treat this as a file not found condition.

      --  We skip this test in multiple unit per file mode since in this
      --  case we can have multiple units from the same source file.

      if Unum > Units.Last and then Get_Unit_Index (Uname_Actual) = 0 then
         for J in Units.First .. Units.Last loop
            if Fname = Units.Table (J).Unit_File_Name then
               if Debug_Flag_L then
                  Write_Str ("  file does not contain unit, Unit_Number = ");
                  Write_Int (Int (Unum));
                  Write_Eol;
                  Write_Eol;
               end if;

               if Present (Error_Node) then
                  Get_Name_String (Fname);

                  if Is_Predefined_File_Name (Fname) then
                     Error_Msg_Unit_1 := Uname_Actual;
                     Error_Msg
                       ("$$ is not a language defined unit", Load_Msg_Sloc);
                  else
                     Error_Msg_File_1 := Fname;
                     Error_Msg_Unit_1 := Uname_Actual;
                     Error_Msg ("File{ does not contain unit$", Load_Msg_Sloc);
                  end if;

                  Write_Dependency_Chain;
                  Unum := No_Unit;
                  goto Done;

               else
                  Unum := No_Unit;
                  goto Done;
               end if;
            end if;
         end loop;
      end if;

      --  If we are proceeding with load, then make load stack entry,
      --  and indicate the kind of with_clause responsible for the load.

      Load_Stack.Increment_Last;
      Load_Stack.Table (Load_Stack.Last) := (Unum, With_Node);

      --  Case of entry already in table

      if Unum <= Units.Last then

         --  Here is where we check for a circular dependency, which is
         --  an attempt to load a unit which is currently in the process
         --  of being loaded. We do *not* care about a circular chain that
         --  leads back to a body, because this kind of circular dependence
         --  legitimately occurs (e.g. two package bodies that contain
         --  inlined subprogram referenced by the other).

         --  Ada 2005 (AI-50217): We also ignore limited_with clauses, because
         --  their purpose is precisely to create legal circular structures.

         if Loading (Unum)
           and then (Is_Spec_Name (Units.Table (Unum).Unit_Name)
                       or else Acts_As_Spec (Units.Table (Unum).Cunit))
           and then (Nkind (Error_Node) /= N_With_Clause
                       or else not Limited_Present (Error_Node))
           and then not From_Limited_With_Chain
         then
            if Debug_Flag_L then
               Write_Str ("  circular dependency encountered");
               Write_Eol;
            end if;

            if Present (Error_Node) then
               Error_Msg ("circular unit dependency", Load_Msg_Sloc);
               Write_Dependency_Chain;
            else
               Load_Stack.Decrement_Last;
            end if;

            Unum := No_Unit;
            goto Done;
         end if;

         if Debug_Flag_L then
            Write_Str ("  unit already in file table, Unit_Number = ");
            Write_Int (Int (Unum));
            Write_Eol;
         end if;

         Load_Stack.Decrement_Last;
         goto Done;

      --  Unit is not already in table, so try to open the file

      else
         if Debug_Flag_L then
            Write_Str ("  attempt unit load, Unit_Number = ");
            Write_Int (Int (Unum));
            Write_Eol;
         end if;

         Src_Ind := Load_Source_File (Fname);

         --  Make a partial entry in the file table, used even in the file not
         --  found case to print the dependency chain including the last entry

         Units.Increment_Last;
         Init_Unit_Name (Unum, Uname_Actual);

         --  File was found

         if Src_Ind > No_Source_File then
            Units.Table (Unum) :=
              (Cunit                  => Empty,
               Cunit_Entity           => Empty,
               Dependency_Num         => 0,
               Dynamic_Elab           => False,
               Error_Location         => Sloc (Error_Node),
               Expected_Unit          => Uname_Actual,
               Fatal_Error            => None,
               Generate_Code          => False,
               Has_RACW               => False,
               Filler                 => False,
               Ident_String           => Empty,

               Is_Predefined_Renaming => Ren_Name,
               Is_Predefined_Unit     => Pre_Name or Ren_Name,
               Is_Internal_Unit       => Pre_Name or Ren_Name or GNAT_Name,
               Filler2                => False,

               Loading                => True,
               Main_Priority          => Default_Main_Priority,
               Main_CPU               => Default_Main_CPU,
               Primary_Stack_Count    => 0,
               Sec_Stack_Count        => 0,
               Munit_Index            => 0,
               No_Elab_Code_All       => False,
               Serial_Number          => 0,
               Source_Index           => Src_Ind,
               Unit_File_Name         => Fname,
               Unit_Name              => Uname_Actual,
               Version                => Source_Checksum (Src_Ind),
               OA_Setting             => 'O');

            --  Parse the new unit

            declare
               Save_Index : constant Nat     := Multiple_Unit_Index;
               Save_PMES  : constant Boolean := Parsing_Main_Extended_Source;

            begin
               Multiple_Unit_Index := Get_Unit_Index (Uname_Actual);
               Units.Table (Unum).Munit_Index := Multiple_Unit_Index;
               Initialize_Scanner (Unum, Source_Index (Unum));

               if Calling_Unit = Main_Unit and then Subunit then
                  Parsing_Main_Extended_Source := True;
               end if;

               Discard_List (Par (Configuration_Pragmas => False));

               Parsing_Main_Extended_Source := Save_PMES;

               Multiple_Unit_Index := Save_Index;
               Set_Loading (Unum, False);
            end;

            --  If spec is irrelevant, then post errors and quit

            if Corr_Body /= No_Unit
              and then Spec_Is_Irrelevant (Unum, Corr_Body)
            then
               Error_Msg_File_1 := Unit_File_Name (Corr_Body);
               Error_Msg
                 ("cannot compile subprogram in file {!", Load_Msg_Sloc);
               Error_Msg_File_1 := Unit_File_Name (Unum);
               Error_Msg
                 ("\incorrect spec in file { must be removed first!",
                  Load_Msg_Sloc);
               Unum := No_Unit;
               goto Done;
            end if;

            --  If loaded unit had an error, then caller inherits setting

            if Present (Error_Node) then
               case Units.Table (Unum).Fatal_Error is

                  --  Nothing to do if with'ed unit had no error

                  when None =>
                     null;

                  --  If with'ed unit had a detected fatal error, propagate it

                  when Error_Detected =>
                     Units.Table (Calling_Unit).Fatal_Error := Error_Detected;

                  --  If with'ed unit had an ignored error, then propagate it
                  --  but do not overide an existring setting.

                  when Error_Ignored =>
                     if Units.Table (Calling_Unit).Fatal_Error = None then
                        Units.Table (Calling_Unit).Fatal_Error :=
                                                               Error_Ignored;
                     end if;
               end case;
            end if;

            --  Remove load stack entry and return the entry in the file table

            Load_Stack.Decrement_Last;

            --  All done, return unit number

            goto Done;

         --  Case of file not found

         else
            if Debug_Flag_L then
               if Src_Ind = No_Access_To_Source_File then
                  Write_Str ("  no read access to file, load failed");
               else
                  Write_Str ("  file was not found, load failed");
               end if;

               Write_Eol;
            end if;

            --  Generate message if unit required

            if Required then
               Get_Name_String (Fname);

               if Is_Predefined_File_Name (Fname) then

                  --  This is a predefined library unit which is not present
                  --  in the run time. If a predefined unit is not available
                  --  it may very likely be the case that there is also pragma
                  --  Restriction forbidding its usage. This is typically the
                  --  case when building a configurable run time, where the
                  --  usage of certain run-time units is restricted by means
                  --  of both the corresponding pragma Restriction (such as
                  --  No_Calendar), and by not including the unit. Hence, we
                  --  check whether this predefined unit is forbidden, so that
                  --  the message about the restriction violation is generated,
                  --  if needed.

                  if Present (Error_Node) then
                     Check_Restricted_Unit (Load_Name, Error_Node);
                  end if;

                  Error_Msg_Unit_1 := Uname_Actual;
                  Error_Msg -- CODEFIX
                    ("$$ is not a predefined library unit", Load_Msg_Sloc);

               else
                  Error_Msg_File_1 := Fname;

                  if Src_Ind = No_Access_To_Source_File then
                     Error_Msg ("no read access to file{", Load_Msg_Sloc);
                  else
                     Error_Msg ("file{ not found", Load_Msg_Sloc);
                  end if;
               end if;

               Write_Dependency_Chain;

               --  Remove unit from stack, to avoid cascaded errors on
               --  subsequent missing files.

               Load_Stack.Decrement_Last;
               Remove_Unit (Unum);

            --  If unit not required, remove load stack entry and the junk
            --  file table entry, and return No_Unit to indicate not found,

            else
               Load_Stack.Decrement_Last;
               Remove_Unit (Unum);
            end if;

            Unum := No_Unit;
            goto Done;
         end if;
      end if;

      --  Here to exit, with result in Unum

      <<Done>>
      Parsing_Main_Extended_Source := Save_PMES;
      Cunit_Boolean_Restrictions_Restore (Save_Cunit_Restrictions);
      return Unum;
   end Load_Unit;

   --------------------------
   -- Make_Child_Decl_Unit --
   --------------------------

   procedure Make_Child_Decl_Unit (N : Node_Id) is
      Unit_Decl : constant Node_Id          := Library_Unit (N);
      Unit_Num  : constant Unit_Number_Type := Get_Cunit_Unit_Number (N);

   begin
      Units.Increment_Last;
      Units.Table (Units.Last) := Units.Table (Unit_Num);
      Units.Table (Units.Last).Cunit := Unit_Decl;
      Units.Table (Units.Last).Cunit_Entity  :=
        Defining_Identifier
          (Defining_Unit_Name (Specification (Unit (Unit_Decl))));
      Init_Unit_Name (Units.Last, Get_Spec_Name (Unit_Name (Unit_Num)));

      --  The library unit created for of a child subprogram unit plays no
      --  role in code generation and binding, so label it accordingly.

      Units.Table (Units.Last).Generate_Code := False;
      Set_Has_No_Elaboration_Code (Unit_Decl);
   end Make_Child_Decl_Unit;

   ------------------------
   -- Make_Instance_Unit --
   ------------------------

   --  If the unit is an instance, it appears as a package declaration, but
   --  contains both declaration and body of the instance. The body becomes
   --  the main unit of the compilation, and the declaration is inserted
   --  at the end of the unit table. The main unit now has the name of a
   --  body, which is constructed from the name of the original spec,
   --  and is attached to the compilation node of the original unit. The
   --  declaration has been attached to a new compilation unit node, and
   --  code will have to be generated for it.

   procedure Make_Instance_Unit (N : Node_Id; In_Main : Boolean) is
      Sind  : constant Source_File_Index := Source_Index (Main_Unit);

   begin
      Units.Increment_Last;

      if In_Main then
         Units.Table (Units.Last)               := Units.Table (Main_Unit);
         Units.Table (Units.Last).Cunit         := Library_Unit (N);
         Units.Table (Units.Last).Generate_Code := True;
         Init_Unit_Name (Units.Last, Unit_Name (Main_Unit));

         Units.Table (Main_Unit).Cunit          := N;
         Units.Table (Main_Unit).Version        := Source_Checksum (Sind);
         Init_Unit_Name (Main_Unit,
           Get_Body_Name
             (Unit_Name (Get_Cunit_Unit_Number (Library_Unit (N)))));

      else
         --  Duplicate information from instance unit, for the body. The unit
         --  node N has been rewritten as a body, but it was placed in the
         --  units table when first loaded as a declaration.

         Units.Table (Units.Last) := Units.Table (Get_Cunit_Unit_Number (N));
         Units.Table (Units.Last).Cunit := Library_Unit (N);
      end if;
   end Make_Instance_Unit;

   ------------------------
   -- Spec_Is_Irrelevant --
   ------------------------

   function Spec_Is_Irrelevant
     (Spec_Unit : Unit_Number_Type;
      Body_Unit : Unit_Number_Type) return Boolean
   is
      Sunit : constant Node_Id := Cunit (Spec_Unit);
      Bunit : constant Node_Id := Cunit (Body_Unit);

   begin
      --  The spec is irrelevant if the body is a subprogram body, and the spec
      --  is other than a subprogram spec or generic subprogram spec. Note that
      --  the names must be the same, we don't need to check that, because we
      --  already know that from the fact that the file names are the same.

      return
         Nkind (Unit (Bunit)) = N_Subprogram_Body
           and then Nkind (Unit (Sunit)) /= N_Subprogram_Declaration
           and then Nkind (Unit (Sunit)) /= N_Generic_Subprogram_Declaration;
   end Spec_Is_Irrelevant;

   --------------------
   -- Version_Update --
   --------------------

   procedure Version_Update (U : Node_Id; From : Node_Id) is
      Unum  : constant Unit_Number_Type := Get_Cunit_Unit_Number (U);
      Fnum  : constant Unit_Number_Type := Get_Cunit_Unit_Number (From);
   begin
      if Source_Index (Fnum) > No_Source_File then
         Units.Table (Unum).Version :=
           Units.Table (Unum).Version
             xor
              Source_Checksum (Source_Index (Fnum));
      end if;
   end Version_Update;

   ----------------------------
   -- Write_Dependency_Chain --
   ----------------------------

   procedure Write_Dependency_Chain is
   begin
      --  The dependency chain is only written if it is at least two entries
      --  deep, otherwise it is trivial (the main unit depending on a unit
      --  that it obviously directly depends on).

      if Load_Stack.Last - 1 > Load_Stack.First then
         for U in Load_Stack.First .. Load_Stack.Last - 1 loop
            Error_Msg_Unit_1 :=
              Unit_Name (Load_Stack.Table (U).Unit_Number);
            Error_Msg_Unit_2 :=
              Unit_Name (Load_Stack.Table (U + 1).Unit_Number);
            Error_Msg ("$ depends on $!", Load_Msg_Sloc);
         end loop;
      end if;
   end Write_Dependency_Chain;

end Lib.Load;
