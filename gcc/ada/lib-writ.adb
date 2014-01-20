------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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

with ALI;      use ALI;
with Atree;    use Atree;
with Casing;   use Casing;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Lib.Util; use Lib.Util;
with Lib.Xref; use Lib.Xref;
with Nlists;   use Nlists;
with Gnatvsn;  use Gnatvsn;
with Opt;      use Opt;
with Osint;    use Osint;
with Osint.C;  use Osint.C;
with Output;   use Output;
with Par;
with Par_SCO;  use Par_SCO;
with Restrict; use Restrict;
with Rident;   use Rident;
with Scn;      use Scn;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Uname;    use Uname;

with System.Case_Util; use System.Case_Util;
with System.WCh_Con;   use System.WCh_Con;

package body Lib.Writ is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Write_Unit_Name (N : Node_Id);
   --  Used to write out the unit name for R (pragma Restriction) lines
   --  for uses of Restriction (No_Dependence => unit-name).

   ----------------------------------
   -- Add_Preprocessing_Dependency --
   ----------------------------------

   procedure Add_Preprocessing_Dependency (S : Source_File_Index) is
   begin
      Units.Increment_Last;
      Units.Table (Units.Last) :=
        (Unit_File_Name    => File_Name (S),
         Unit_Name         => No_Unit_Name,
         Expected_Unit     => No_Unit_Name,
         Source_Index      => S,
         Cunit             => Empty,
         Cunit_Entity      => Empty,
         Dependency_Num    => 0,
         Dynamic_Elab      => False,
         Fatal_Error       => False,
         Generate_Code     => False,
         Has_Allocator     => False,
         Has_RACW          => False,
         Is_Compiler_Unit  => False,
         Ident_String      => Empty,
         Loading           => False,
         Main_Priority     => -1,
         Main_CPU          => -1,
         Munit_Index       => 0,
         Serial_Number     => 0,
         Version           => 0,
         Error_Location    => No_Location,
         OA_Setting        => 'O',
         SPARK_Mode_Pragma => Empty);
   end Add_Preprocessing_Dependency;

   ------------------------------
   -- Ensure_System_Dependency --
   ------------------------------

   procedure Ensure_System_Dependency is
      System_Uname : Unit_Name_Type;
      --  Unit name for system spec if needed for dummy entry

      System_Fname : File_Name_Type;
      --  File name for system spec if needed for dummy entry

   begin
      --  Nothing to do if we already compiled System

      for Unum in Units.First .. Last_Unit loop
         if Units.Table (Unum).Source_Index = System_Source_File_Index then
            return;
         end if;
      end loop;

      --  If no entry for system.ads in the units table, then add a entry
      --  to the units table for system.ads, which will be referenced when
      --  the ali file is generated. We need this because every unit depends
      --  on system as a result of Targparm scanning the system.ads file to
      --  determine the target dependent parameters for the compilation.

      Name_Len := 6;
      Name_Buffer (1 .. 6) := "system";
      System_Uname := Name_To_Unit_Name (Name_Enter);
      System_Fname := File_Name (System_Source_File_Index);

      Units.Increment_Last;
      Units.Table (Units.Last) := (
        Unit_File_Name    => System_Fname,
        Unit_Name         => System_Uname,
        Expected_Unit     => System_Uname,
        Source_Index      => System_Source_File_Index,
        Cunit             => Empty,
        Cunit_Entity      => Empty,
        Dependency_Num    => 0,
        Dynamic_Elab      => False,
        Fatal_Error       => False,
        Generate_Code     => False,
        Has_Allocator     => False,
        Has_RACW          => False,
        Is_Compiler_Unit  => False,
        Ident_String      => Empty,
        Loading           => False,
        Main_Priority     => -1,
        Main_CPU          => -1,
        Munit_Index       => 0,
        Serial_Number     => 0,
        Version           => 0,
        Error_Location    => No_Location,
        OA_Setting        => 'O',
        SPARK_Mode_Pragma => Empty);

      --  Parse system.ads so that the checksum is set right
      --  Style checks are not applied.

      declare
         Save_Mindex : constant Nat := Multiple_Unit_Index;
         Save_Style  : constant Boolean := Style_Check;
      begin
         Multiple_Unit_Index := 0;
         Style_Check := False;
         Initialize_Scanner (Units.Last, System_Source_File_Index);
         Discard_List (Par (Configuration_Pragmas => False));
         Style_Check := Save_Style;
         Multiple_Unit_Index := Save_Mindex;
      end;
   end Ensure_System_Dependency;

   ---------------
   -- Write_ALI --
   ---------------

   procedure Write_ALI (Object : Boolean) is

      ----------------
      -- Local Data --
      ----------------

      Last_Unit : constant Unit_Number_Type := Units.Last;
      --  Record unit number of last unit. We capture this in case we
      --  have to add a dummy entry to the unit table for package System.

      With_Flags : array (Units.First .. Last_Unit) of Boolean;
      --  Array of flags to show which units are with'ed

      Elab_Flags : array (Units.First .. Last_Unit) of Boolean;
      --  Array of flags to show which units have pragma Elaborate set

      Elab_All_Flags : array (Units.First .. Last_Unit) of Boolean;
      --  Array of flags to show which units have pragma Elaborate All set

      Elab_Des_Flags : array (Units.First .. Last_Unit) of Boolean;
      --  Array of flags to show which units have Elaborate_Desirable set

      Elab_All_Des_Flags : array (Units.First .. Last_Unit) of Boolean;
      --  Array of flags to show which units have Elaborate_All_Desirable set

      type Yes_No is (Unknown, Yes, No);
      Implicit_With : array (Units.First .. Last_Unit) of Yes_No;
      --  Indicates if an implicit with has been given for the unit. Yes if
      --  certainly present, no if certainly absent, unkonwn if not known.

      Sdep_Table : Unit_Ref_Table (1 .. Pos (Last_Unit - Units.First + 2));
      --  Sorted table of source dependencies. One extra entry in case we
      --  have to add a dummy entry for System.

      Num_Sdep : Nat := 0;
      --  Number of active entries in Sdep_Table

      flag_compare_debug : Int;
      pragma Import (C, flag_compare_debug);
      --  Import from toplev.c

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Collect_Withs (Cunit : Node_Id);
      --  Collect with lines for entries in the context clause of the
      --  given compilation unit, Cunit.

      procedure Update_Tables_From_ALI_File;
      --  Given an up to date ALI file (see Up_To_Date_ALI_file_Exists
      --  function), update tables from the ALI information, including
      --  specifically the Compilation_Switches table.

      function Up_To_Date_ALI_File_Exists return Boolean;
      --  If there exists an ALI file that is up to date, then this function
      --  initializes the tables in the ALI spec to contain information on
      --  this file (using Scan_ALI) and returns True. If no file exists,
      --  or the file is not up to date, then False is returned.

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type);
      --  Write out the library information for one unit for which code is
      --  generated (includes unit line and with lines).

      procedure Write_With_Lines;
      --  Write out with lines collected by calls to Collect_Withs

      -------------------
      -- Collect_Withs --
      -------------------

      procedure Collect_Withs (Cunit : Node_Id) is
         Item : Node_Id;
         Unum : Unit_Number_Type;

      begin
         Item := First (Context_Items (Cunit));
         while Present (Item) loop

            --  Process with clause

            --  Ada 2005 (AI-50217): limited with_clauses do not create
            --  dependencies, but must be recorded as components of the
            --  partition, in case there is no regular with_clause for
            --  the unit anywhere else.

            if Nkind (Item) = N_With_Clause then
               Unum := Get_Cunit_Unit_Number (Library_Unit (Item));
               With_Flags (Unum) := True;

               if not Limited_Present (Item) then
                  if Elaborate_Present (Item) then
                     Elab_Flags (Unum) := True;
                  end if;

                  if Elaborate_All_Present (Item) then
                     Elab_All_Flags (Unum) := True;
                  end if;

                  if Elaborate_All_Desirable (Item) then
                     Elab_All_Des_Flags (Unum) := True;
                  end if;

                  if Elaborate_Desirable (Item) then
                     Elab_Des_Flags (Unum) := True;
                  end if;

               else
                  Set_From_Limited_With (Cunit_Entity (Unum));
               end if;

               if Implicit_With (Unum) /= Yes then
                  if Implicit_With_From_Instantiation (Item) then
                     Implicit_With (Unum) := Yes;
                  else
                     Implicit_With (Unum) := No;
                  end if;
               end if;
            end if;

            Next (Item);
         end loop;
      end Collect_Withs;

      --------------------------------
      -- Up_To_Date_ALI_File_Exists --
      --------------------------------

      function Up_To_Date_ALI_File_Exists return Boolean is
         Name : File_Name_Type;
         Text : Text_Buffer_Ptr;
         Id   : Sdep_Id;
         Sind : Source_File_Index;

      begin
         Opt.Check_Object_Consistency := True;
         Read_Library_Info (Name, Text);

         --  Return if we could not find an ALI file

         if Text = null then
            return False;
         end if;

         --  Return if ALI file has bad format

         Initialize_ALI;

         if Scan_ALI (Name, Text, False, Err => True) = No_ALI_Id then
            return False;
         end if;

         --  If we have an OK ALI file, check if it is up to date
         --  Note that we assume that the ALI read has all the entries
         --  we have in our table, plus some additional ones (that can
         --  come from expansion).

         Id := First_Sdep_Entry;
         for J in 1 .. Num_Sdep loop
            Sind := Units.Table (Sdep_Table (J)).Source_Index;

            while Sdep.Table (Id).Sfile /= File_Name (Sind) loop
               if Id = Sdep.Last then
                  return False;
               else
                  Id := Id + 1;
               end if;
            end loop;

            if Sdep.Table (Id).Stamp /= Time_Stamp (Sind) then
               return False;
            end if;
         end loop;

         return True;
      end Up_To_Date_ALI_File_Exists;

      ---------------------------------
      -- Update_Tables_From_ALI_File --
      ---------------------------------

      procedure Update_Tables_From_ALI_File is
      begin
         --  Build Compilation_Switches table

         Compilation_Switches.Init;

         for J in First_Arg_Entry .. Args.Last loop
            Compilation_Switches.Increment_Last;
            Compilation_Switches.Table (Compilation_Switches.Last) :=
              Args.Table (J);
         end loop;
      end Update_Tables_From_ALI_File;

      ----------------------------
      -- Write_Unit_Information --
      ----------------------------

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type) is
         Unode : constant Node_Id   := Cunit (Unit_Num);
         Ukind : constant Node_Kind := Nkind (Unit (Unode));
         Uent  : constant Entity_Id := Cunit_Entity (Unit_Num);
         Pnode : Node_Id;

      begin
         Write_Info_Initiate ('U');
         Write_Info_Char (' ');
         Write_Info_Name (Unit_Name (Unit_Num));
         Write_Info_Tab (25);
         Write_Info_Name (Unit_File_Name (Unit_Num));

         Write_Info_Tab (49);
         Write_Info_Str (Version_Get (Unit_Num));

         --  Add BD parameter if Elaborate_Body pragma desirable

         if Ekind (Uent) = E_Package
           and then Elaborate_Body_Desirable (Uent)
         then
            Write_Info_Str (" BD");
         end if;

         --  Add BN parameter if body needed for SAL

         if (Is_Subprogram (Uent)
              or else Ekind (Uent) = E_Package
              or else Is_Generic_Unit (Uent))
           and then Body_Needed_For_SAL (Uent)
         then
            Write_Info_Str (" BN");
         end if;

         if Dynamic_Elab (Unit_Num) then
            Write_Info_Str (" DE");
         end if;

         --  Set the Elaborate_Body indication if either an explicit pragma
         --  was present, or if this is an instantiation.

         if Has_Pragma_Elaborate_Body (Uent)
           or else (Ukind = N_Package_Declaration
                     and then Is_Generic_Instance (Uent)
                     and then Present (Corresponding_Body (Unit (Unode))))
         then
            Write_Info_Str (" EB");
         end if;

         --  Now see if we should tell the binder that an elaboration entity
         --  is present, which must be set to true during elaboration.
         --  We generate the indication if the following condition is met:

         --  If this is a spec ...

         if (Is_Subprogram (Uent)
               or else
             Ekind (Uent) = E_Package
               or else
             Is_Generic_Unit (Uent))

            --  and an elaboration entity was declared ...

            and then Present (Elaboration_Entity (Uent))

            --  and either the elaboration flag is required ...

            and then
              (Elaboration_Entity_Required (Uent)

               --  or this unit has elaboration code ...

               or else not Has_No_Elaboration_Code (Unode)

               --  or this unit has a separate body and this
               --  body has elaboration code.

               or else
                 (Ekind (Uent) = E_Package
                   and then Present (Body_Entity (Uent))
                   and then
                     not Has_No_Elaboration_Code
                           (Parent
                             (Declaration_Node
                               (Body_Entity (Uent))))))
         then
            if Convention (Uent) = Convention_CIL then

               --  Special case for generic CIL packages which never have
               --  elaboration code

               Write_Info_Str (" NE");

            else
               Write_Info_Str (" EE");
            end if;
         end if;

         if Has_No_Elaboration_Code (Unode) then
            Write_Info_Str (" NE");
         end if;

         Write_Info_Str (" O");
         Write_Info_Char (OA_Setting (Unit_Num));

         if Ekind_In (Uent, E_Package, E_Package_Body)
           and then Present (Finalizer (Uent))
         then
            Write_Info_Str (" PF");
         end if;

         if Is_Preelaborated (Uent) then
            Write_Info_Str (" PR");
         end if;

         if Is_Pure (Uent) then
            Write_Info_Str (" PU");
         end if;

         if Has_RACW (Unit_Num) then
            Write_Info_Str (" RA");
         end if;

         if Is_Remote_Call_Interface (Uent) then
            Write_Info_Str (" RC");
         end if;

         if Is_Remote_Types (Uent) then
            Write_Info_Str (" RT");
         end if;

         if Is_Shared_Passive (Uent) then
            Write_Info_Str (" SP");
         end if;

         if Ukind = N_Subprogram_Declaration
           or else Ukind = N_Subprogram_Body
         then
            Write_Info_Str (" SU");

         elsif Ukind = N_Package_Declaration
                 or else
               Ukind = N_Package_Body
         then
            --  If this is a wrapper package for a subprogram instantiation,
            --  the user view is the subprogram. Note that in this case the
            --  ali file contains both the spec and body of the instance.

            if Is_Wrapper_Package (Uent) then
               Write_Info_Str (" SU");
            else
               Write_Info_Str (" PK");
            end if;

         elsif Ukind = N_Generic_Package_Declaration then
            Write_Info_Str (" PK");

         end if;

         if Ukind in N_Generic_Declaration
           or else
             (Present (Library_Unit (Unode))
                and then
              Nkind (Unit (Library_Unit (Unode))) in N_Generic_Declaration)
         then
            Write_Info_Str (" GE");
         end if;

         if not Is_Internal_File_Name (Unit_File_Name (Unit_Num), True) then
            case Identifier_Casing (Source_Index (Unit_Num)) is
               when All_Lower_Case => Write_Info_Str (" IL");
               when All_Upper_Case => Write_Info_Str (" IU");
               when others         => null;
            end case;

            case Keyword_Casing (Source_Index (Unit_Num)) is
               when Mixed_Case     => Write_Info_Str (" KM");
               when All_Upper_Case => Write_Info_Str (" KU");
               when others         => null;
            end case;
         end if;

         if Initialize_Scalars or else Invalid_Value_Used then
            Write_Info_Str (" IS");
         end if;

         Write_Info_EOL;

         --  Generate with lines, first those that are directly with'ed

         for J in With_Flags'Range loop
            With_Flags         (J) := False;
            Elab_Flags         (J) := False;
            Elab_All_Flags     (J) := False;
            Elab_Des_Flags     (J) := False;
            Elab_All_Des_Flags (J) := False;
            Implicit_With      (J) := Unknown;
         end loop;

         Collect_Withs (Unode);

         --  For a body, we must also check for any subunits which belong to
         --  it and which have context clauses of their own, since these
         --  with'ed units are part of its own elaboration dependencies.

         if Nkind (Unit (Unode)) in N_Unit_Body then
            for S in Units.First .. Last_Unit loop

               --  We are only interested in subunits.
               --  For preproc. data and def. files, Cunit is Empty, so
               --  we need to test that first.

               if Cunit (S) /= Empty
                 and then Nkind (Unit (Cunit (S))) = N_Subunit
               then
                  Pnode := Library_Unit (Cunit (S));

                  --  In gnatc mode, the errors in the subunits will not
                  --  have been recorded, but the analysis of the subunit
                  --  may have failed. There is no information to add to
                  --  ALI file in this case.

                  if No (Pnode) then
                     exit;
                  end if;

                  --  Find ultimate parent of the subunit

                  while Nkind (Unit (Pnode)) = N_Subunit loop
                     Pnode := Library_Unit (Pnode);
                  end loop;

                  --  See if it belongs to current unit, and if so, include
                  --  its with_clauses.

                  if Pnode = Unode then
                     Collect_Withs (Cunit (S));
                  end if;
               end if;
            end loop;
         end if;

         Write_With_Lines;

         --  Generate the linker option lines

         for J in 1 .. Linker_Option_Lines.Last loop

            --  Pragma Linker_Options is not allowed in predefined generic
            --  units. This is because they won't be read, due to the fact that
            --  with lines for generic units lack the file name and lib name
            --  parameters (see Lib_Writ spec for an explanation).

            if Is_Generic_Unit (Cunit_Entity (Main_Unit))
              and then
                Is_Predefined_File_Name (Unit_File_Name (Current_Sem_Unit))
              and then Linker_Option_Lines.Table (J).Unit = Unit_Num
            then
               Set_Standard_Error;
               Write_Line
                 ("linker options not allowed in predefined generic unit");
               raise Unrecoverable_Error;
            end if;

            --  Output one linker option line

            declare
               S : Linker_Option_Entry renames Linker_Option_Lines.Table (J);
            begin
               if S.Unit = Unit_Num then
                  Write_Info_Initiate ('L');
                  Write_Info_Char (' ');
                  Write_Info_Slit (S.Option);
                  Write_Info_EOL;
               end if;
            end;
         end loop;

         --  Output notes

         for J in 1 .. Notes.Last loop
            declare
               N : constant Node_Id          := Notes.Table (J).Pragma_Node;
               L : constant Source_Ptr       := Sloc (N);
               U : constant Unit_Number_Type := Notes.Table (J).Unit;
               C : Character;

            begin
               if U = Unit_Num then
                  Write_Info_Initiate ('N');
                  Write_Info_Char (' ');

                  case Chars (Pragma_Identifier (N)) is
                     when Name_Annotate =>
                        C := 'A';
                     when Name_Comment =>
                        C := 'C';
                     when Name_Ident =>
                        C := 'I';
                     when Name_Title =>
                        C := 'T';
                     when Name_Subtitle =>
                        C := 'S';
                     when others =>
                        raise Program_Error;
                  end case;

                  Write_Info_Char (C);
                  Write_Info_Int (Int (Get_Logical_Line_Number (L)));
                  Write_Info_Char (':');
                  Write_Info_Int (Int (Get_Column_Number (L)));

                  declare
                     A : Node_Id;

                  begin
                     A := First (Pragma_Argument_Associations (N));
                     while Present (A) loop
                        Write_Info_Char (' ');

                        if Chars (A) /= No_Name then
                           Write_Info_Name (Chars (A));
                           Write_Info_Char (':');
                        end if;

                        declare
                           Expr : constant Node_Id := Expression (A);

                        begin
                           if Nkind (Expr) = N_Identifier then
                              Write_Info_Name (Chars (Expr));

                           elsif Nkind (Expr) = N_Integer_Literal
                             and then Is_Static_Expression (Expr)
                           then
                              Write_Info_Uint (Intval (Expr));

                           elsif Nkind (Expr) = N_String_Literal
                             and then Is_Static_Expression (Expr)
                           then
                              Write_Info_Slit (Strval (Expr));

                           else
                              Write_Info_Str ("<expr>");
                           end if;
                        end;

                        Next (A);
                     end loop;
                  end;

                  Write_Info_EOL;
               end if;
            end;
         end loop;
      end Write_Unit_Information;

      ----------------------
      -- Write_With_Lines --
      ----------------------

      procedure Write_With_Lines is
         With_Table : Unit_Ref_Table (1 .. Pos (Last_Unit - Units.First + 1));
         Num_Withs  : Int := 0;
         Unum       : Unit_Number_Type;
         Cunit      : Node_Id;
         Uname      : Unit_Name_Type;
         Fname      : File_Name_Type;
         Pname      : constant Unit_Name_Type :=
                        Get_Parent_Spec_Name (Unit_Name (Main_Unit));
         Body_Fname : File_Name_Type;
         Body_Index : Nat;

         procedure Write_With_File_Names
           (Nam : in out File_Name_Type;
            Idx : Nat);
         --  Write source file name Nam and ALI file name for unit index Idx.
         --  Possibly change Nam to lowercase (generating a new file name).

         --------------------------
         -- Write_With_File_Name --
         --------------------------

         procedure Write_With_File_Names
           (Nam : in out File_Name_Type;
            Idx : Nat)
         is
         begin
            if not File_Names_Case_Sensitive then
               Get_Name_String (Nam);
               To_Lower (Name_Buffer (1 .. Name_Len));
               Nam := Name_Find;
            end if;

            Write_Info_Name (Nam);
            Write_Info_Tab (49);
            Write_Info_Name (Lib_File_Name (Nam, Idx));
         end Write_With_File_Names;

      --  Start of processing for Write_With_Lines

      begin
         --  Loop to build the with table. A with on the main unit itself
         --  is ignored (AARM 10.2(14a)). Such a with-clause can occur if
         --  the main unit is a subprogram with no spec, and a subunit of
         --  it unnecessarily withs the parent.

         for J in Units.First + 1 .. Last_Unit loop

            --  Add element to with table if it is with'ed or if it is the
            --  parent spec of the main unit (case of main unit is a child
            --  unit). The latter with is not needed for semantic purposes,
            --  but is required by the binder for elaboration purposes.
            --  For preproc. data and def. files, there is no Unit_Name,
            --  check for that first.

            if Unit_Name (J) /= No_Unit_Name
              and then (With_Flags (J) or else Unit_Name (J) = Pname)
            then
               Num_Withs := Num_Withs + 1;
               With_Table (Num_Withs) := J;
            end if;
         end loop;

         --  Sort and output the table

         Sort (With_Table (1 .. Num_Withs));

         for J in 1 .. Num_Withs loop
            Unum   := With_Table (J);
            Cunit  := Units.Table (Unum).Cunit;
            Uname  := Units.Table (Unum).Unit_Name;
            Fname  := Units.Table (Unum).Unit_File_Name;

            if Implicit_With (Unum) = Yes then
               Write_Info_Initiate ('Z');

            elsif Ekind (Cunit_Entity (Unum)) = E_Package
              and then From_Limited_With (Cunit_Entity (Unum))
            then
               Write_Info_Initiate ('Y');

            else
               Write_Info_Initiate ('W');
            end if;

            Write_Info_Char (' ');
            Write_Info_Name (Uname);

            --  Now we need to figure out the names of the files that contain
            --  the with'ed unit. These will usually be the files for the body,
            --  except in the case of a package that has no body. Note that we
            --  have a specific exemption here for predefined library generics
            --  (see comments for Generic_May_Lack_ALI). We do not generate
            --  dependency upon the ALI file for such units. Older compilers
            --  used to not support generating code (and ALI) for generics, and
            --  we want to avoid having different processing (namely, different
            --  lists of files to be compiled) for different stages of the
            --  bootstrap.

            if not ((Nkind (Unit (Cunit)) in N_Generic_Declaration
                      or else
                     Nkind (Unit (Cunit)) in N_Generic_Renaming_Declaration)
                    and then Generic_May_Lack_ALI (Fname))

              --  In SPARK mode, always generate the dependencies on ALI
              --  files, which are required to compute frame conditions
              --  of subprograms.

              or else GNATprove_Mode
            then
               Write_Info_Tab (25);

               if Is_Spec_Name (Uname) then
                  Body_Fname :=
                    Get_File_Name
                      (Get_Body_Name (Uname),
                       Subunit => False, May_Fail => True);

                  Body_Index :=
                    Get_Unit_Index
                      (Get_Body_Name (Uname));

                  if Body_Fname = No_File then
                     Body_Fname := Get_File_Name (Uname, Subunit => False);
                     Body_Index := Get_Unit_Index (Uname);
                  end if;

               else
                  Body_Fname := Get_File_Name (Uname, Subunit => False);
                  Body_Index := Get_Unit_Index (Uname);
               end if;

               --  A package is considered to have a body if it requires
               --  a body or if a body is present in Ada 83 mode.

               if Body_Required (Cunit)
                 or else (Ada_Version = Ada_83
                           and then Full_Source_Name (Body_Fname) /= No_File)
               then
                  Write_With_File_Names (Body_Fname, Body_Index);
               else
                  Write_With_File_Names (Fname, Munit_Index (Unum));
               end if;

               if Ekind (Cunit_Entity (Unum)) = E_Package
                  and then From_Limited_With (Cunit_Entity (Unum))
               then
                  null;
               else
                  if Elab_Flags (Unum) then
                     Write_Info_Str ("  E");
                  end if;

                  if Elab_All_Flags (Unum) then
                     Write_Info_Str ("  EA");
                  end if;

                  if Elab_Des_Flags (Unum) then
                     Write_Info_Str ("  ED");
                  end if;

                  if Elab_All_Des_Flags (Unum) then
                     Write_Info_Str ("  AD");
                  end if;
               end if;
            end if;

            Write_Info_EOL;
         end loop;

         --  Finally generate the special lines for cases of Restriction_Set
         --  with No_Dependence and no restriction present.

         declare
            Unam : Unit_Name_Type;

         begin
            for J in Restriction_Set_Dependences.First ..
                     Restriction_Set_Dependences.Last
            loop
               Unam := Restriction_Set_Dependences.Table (J);

               --  Don't need an entry if already in the unit table

               for U in 0 .. Last_Unit loop
                  if Unit_Name (U) = Unam then
                     goto Continue;
                  end if;
               end loop;

               --  Otherwise generate the entry

               Write_Info_Initiate ('W');
               Write_Info_Char (' ');
               Write_Info_Name (Unam);
               Write_Info_EOL;

            <<Continue>>
               null;
            end loop;
         end;
      end Write_With_Lines;

   --  Start of processing for Write_ALI

   begin
      --  We never write an ALI file if the original operating mode was
      --  syntax-only (-gnats switch used in compiler invocation line)

      if Original_Operating_Mode = Check_Syntax
        or flag_compare_debug /= 0
      then
         return;
      end if;

      --  Generation of ALI files may be disabled, e.g. for formal verification
      --  back-end.

      if Disable_ALI_File then
         return;
      end if;

      --  Build sorted source dependency table. We do this right away, because
      --  it is referenced by Up_To_Date_ALI_File_Exists.

      for Unum in Units.First .. Last_Unit loop
         if Cunit_Entity (Unum) = Empty
           or else not From_Limited_With (Cunit_Entity (Unum))
         then
            Num_Sdep := Num_Sdep + 1;
            Sdep_Table (Num_Sdep) := Unum;
         end if;
      end loop;

      --  Sort the table so that the D lines are in order

      Lib.Sort (Sdep_Table (1 .. Num_Sdep));

      --  If we are not generating code, and there is an up to date ALI file
      --  file accessible, read it, and acquire the compilation arguments from
      --  this file. In GNATprove mode, always generate the ALI file, which
      --  contains a special section for formal verification.

      if Operating_Mode /= Generate_Code and then not GNATprove_Mode then
         if Up_To_Date_ALI_File_Exists then
            Update_Tables_From_ALI_File;
            return;
         end if;
      end if;

      --  Otherwise acquire compilation arguments and prepare to write
      --  out a new ali file.

      Create_Output_Library_Info;

      --  Output version line

      Write_Info_Initiate ('V');
      Write_Info_Str (" """);
      Write_Info_Str (Verbose_Library_Version);
      Write_Info_Char ('"');

      Write_Info_EOL;

      --  Output main program line if this is acceptable main program

      Output_Main_Program_Line : declare
         U : Node_Id := Unit (Units.Table (Main_Unit).Cunit);
         S : Node_Id;

         procedure M_Parameters;
         --  Output parameters for main program line

         ------------------
         -- M_Parameters --
         ------------------

         procedure M_Parameters is
         begin
            if Main_Priority (Main_Unit) /= Default_Main_Priority then
               Write_Info_Char (' ');
               Write_Info_Nat (Main_Priority (Main_Unit));
            end if;

            if Opt.Time_Slice_Set then
               Write_Info_Str (" T=");
               Write_Info_Nat (Opt.Time_Slice_Value);
            end if;

            if Has_Allocator (Main_Unit) then
               Write_Info_Str (" AB");
            end if;

            if Main_CPU (Main_Unit) /= Default_Main_CPU then
               Write_Info_Str (" C=");
               Write_Info_Nat (Main_CPU (Main_Unit));
            end if;

            Write_Info_Str (" W=");
            Write_Info_Char
              (WC_Encoding_Letters (Wide_Character_Encoding_Method));

            Write_Info_EOL;
         end M_Parameters;

      --  Start of processing for Output_Main_Program_Line

      begin
         if Nkind (U) = N_Subprogram_Body
           or else
             (Nkind (U) = N_Package_Body
               and then
                 Nkind (Original_Node (U)) in N_Subprogram_Instantiation)
         then
            --  If the unit is a subprogram instance, the entity for the
            --  subprogram is the alias of the visible entity, which is the
            --  related instance of the wrapper package. We retrieve the
            --  subprogram declaration of the desired entity.

            if Nkind (U) = N_Package_Body then
               U := Parent (Parent (
                   Alias (Related_Instance (Defining_Unit_Name
                     (Specification (Unit (Library_Unit (Parent (U)))))))));
            end if;

            S := Specification (U);

            --  A generic subprogram is never a main program

            if Nkind (U) = N_Subprogram_Body
              and then Present (Corresponding_Spec (U))
              and then
                Ekind_In (Corresponding_Spec (U),
                  E_Generic_Procedure, E_Generic_Function)
            then
               null;

            elsif No (Parameter_Specifications (S)) then
               if Nkind (S) = N_Procedure_Specification then
                  Write_Info_Initiate ('M');
                  Write_Info_Str (" P");
                  M_Parameters;

               else
                  declare
                     Nam : Node_Id := Defining_Unit_Name (S);

                  begin
                     --  If it is a child unit, get its simple name

                     if Nkind (Nam) = N_Defining_Program_Unit_Name then
                        Nam := Defining_Identifier (Nam);
                     end if;

                     if Is_Integer_Type (Etype (Nam)) then
                        Write_Info_Initiate ('M');
                        Write_Info_Str (" F");
                        M_Parameters;
                     end if;
                  end;
               end if;
            end if;
         end if;
      end Output_Main_Program_Line;

      --  Write command argument ('A') lines

      for A in 1 .. Compilation_Switches.Last loop
         Write_Info_Initiate ('A');
         Write_Info_Char (' ');
         Write_Info_Str (Compilation_Switches.Table (A).all);
         Write_Info_Terminate;
      end loop;

      --  Output parameters ('P') line

      Write_Info_Initiate ('P');

      if Compilation_Errors then
         Write_Info_Str (" CE");
      end if;

      if Opt.Detect_Blocking then
         Write_Info_Str (" DB");
      end if;

      if Opt.Float_Format /= ' ' then
         Write_Info_Str (" F");

         if Opt.Float_Format = 'I' then
            Write_Info_Char ('I');

         elsif Opt.Float_Format_Long = 'D' then
            Write_Info_Char ('D');

         else
            Write_Info_Char ('G');
         end if;
      end if;

      if Tasking_Used
        and then not Is_Predefined_File_Name (Unit_File_Name (Main_Unit))
      then
         if Locking_Policy /= ' ' then
            Write_Info_Str  (" L");
            Write_Info_Char (Locking_Policy);
         end if;

         if Queuing_Policy /= ' ' then
            Write_Info_Str  (" Q");
            Write_Info_Char (Queuing_Policy);
         end if;

         if Task_Dispatching_Policy /= ' ' then
            Write_Info_Str  (" T");
            Write_Info_Char (Task_Dispatching_Policy);
            Write_Info_Char (' ');
         end if;
      end if;

      if Partition_Elaboration_Policy /= ' ' then
         Write_Info_Str  (" E");
         Write_Info_Char (Partition_Elaboration_Policy);
      end if;

      if not Object then
         Write_Info_Str (" NO");
      end if;

      if No_Run_Time_Mode then
         Write_Info_Str (" NR");
      end if;

      if Normalize_Scalars then
         Write_Info_Str (" NS");
      end if;

      if Sec_Stack_Used then
         Write_Info_Str (" SS");
      end if;

      if Unreserve_All_Interrupts then
         Write_Info_Str (" UA");
      end if;

      if Exception_Mechanism = Back_End_Exceptions then
         Write_Info_Str (" ZX");
      end if;

      Write_Info_EOL;

      --  Before outputting the restrictions line, update the setting of
      --  the No_Elaboration_Code flag. Violations of this restriction
      --  cannot be detected until after the backend has been called since
      --  it is the backend that sets this flag. We have to check all units
      --  for which we have generated code

      for Unit in Units.First .. Last_Unit loop
         if Units.Table (Unit).Generate_Code
           or else Unit = Main_Unit
         then
            if not Has_No_Elaboration_Code (Cunit (Unit)) then
               Main_Restrictions.Violated (No_Elaboration_Code) := True;
            end if;
         end if;
      end loop;

      --  Positional case (only if debug flag -gnatd.R is set)

      if Debug_Flag_Dot_RR then

         --  Output first restrictions line

         Write_Info_Initiate ('R');
         Write_Info_Char (' ');

         --  First the information for the boolean restrictions

         for R in All_Boolean_Restrictions loop
            if Main_Restrictions.Set (R)
              and then not Restriction_Warnings (R)
            then
               Write_Info_Char ('r');
            elsif Main_Restrictions.Violated (R) then
               Write_Info_Char ('v');
            else
               Write_Info_Char ('n');
            end if;
         end loop;

         --  And now the information for the parameter restrictions

         for RP in All_Parameter_Restrictions loop
            if Main_Restrictions.Set (RP)
              and then not Restriction_Warnings (RP)
            then
               Write_Info_Char ('r');
               Write_Info_Nat (Nat (Main_Restrictions.Value (RP)));
            else
               Write_Info_Char ('n');
            end if;

            if not Main_Restrictions.Violated (RP)
              or else RP not in Checked_Parameter_Restrictions
            then
               Write_Info_Char ('n');
            else
               Write_Info_Char ('v');
               Write_Info_Nat (Nat (Main_Restrictions.Count (RP)));

               if Main_Restrictions.Unknown (RP) then
                  Write_Info_Char ('+');
               end if;
            end if;
         end loop;

         Write_Info_EOL;

      --  Named case (if debug flag -gnatd.R is not set)

      else
         declare
            C : Character;

         begin
            --  Write RN header line with preceding blank line

            Write_Info_EOL;
            Write_Info_Initiate ('R');
            Write_Info_Char ('N');
            Write_Info_EOL;

            --  First the lines for the boolean restrictions

            for R in All_Boolean_Restrictions loop
               if Main_Restrictions.Set (R)
                 and then not Restriction_Warnings (R)
               then
                  C := 'R';
               elsif Main_Restrictions.Violated (R) then
                  C := 'V';
               else
                  goto Continue;
               end if;

               Write_Info_Initiate ('R');
               Write_Info_Char (C);
               Write_Info_Char (' ');
               Write_Info_Str (All_Boolean_Restrictions'Image (R));
               Write_Info_EOL;

            <<Continue>>
               null;
            end loop;
         end;

         --  And now the lines for the parameter restrictions

         for RP in All_Parameter_Restrictions loop
            if Main_Restrictions.Set (RP)
              and then not Restriction_Warnings (RP)
            then
               Write_Info_Initiate ('R');
               Write_Info_Str ("R ");
               Write_Info_Str (All_Parameter_Restrictions'Image (RP));
               Write_Info_Char ('=');
               Write_Info_Nat (Nat (Main_Restrictions.Value (RP)));
               Write_Info_EOL;
            end if;

            if not Main_Restrictions.Violated (RP)
              or else RP not in Checked_Parameter_Restrictions
            then
               null;
            else
               Write_Info_Initiate ('R');
               Write_Info_Str ("V ");
               Write_Info_Str (All_Parameter_Restrictions'Image (RP));
               Write_Info_Char ('=');
               Write_Info_Nat (Nat (Main_Restrictions.Count (RP)));

               if Main_Restrictions.Unknown (RP) then
                  Write_Info_Char ('+');
               end if;

               Write_Info_EOL;
            end if;
         end loop;
      end if;

      --  Output R lines for No_Dependence entries

      for J in No_Dependences.First .. No_Dependences.Last loop
         if In_Extended_Main_Source_Unit (No_Dependences.Table (J).Unit)
           and then not No_Dependences.Table (J).Warn
         then
            Write_Info_Initiate ('R');
            Write_Info_Char (' ');
            Write_Unit_Name (No_Dependences.Table (J).Unit);
            Write_Info_EOL;
         end if;
      end loop;

      --  Output interrupt state lines

      for J in Interrupt_States.First .. Interrupt_States.Last loop
         Write_Info_Initiate ('I');
         Write_Info_Char (' ');
         Write_Info_Nat (Interrupt_States.Table (J).Interrupt_Number);
         Write_Info_Char (' ');
         Write_Info_Char (Interrupt_States.Table (J).Interrupt_State);
         Write_Info_Char (' ');
         Write_Info_Nat
           (Nat (Get_Logical_Line_Number
                   (Interrupt_States.Table (J).Pragma_Loc)));
         Write_Info_EOL;
      end loop;

      --  Output priority specific dispatching lines

      for J in Specific_Dispatching.First .. Specific_Dispatching.Last loop
         Write_Info_Initiate ('S');
         Write_Info_Char (' ');
         Write_Info_Char (Specific_Dispatching.Table (J).Dispatching_Policy);
         Write_Info_Char (' ');
         Write_Info_Nat (Specific_Dispatching.Table (J).First_Priority);
         Write_Info_Char (' ');
         Write_Info_Nat (Specific_Dispatching.Table (J).Last_Priority);
         Write_Info_Char (' ');
         Write_Info_Nat
           (Nat (Get_Logical_Line_Number
                   (Specific_Dispatching.Table (J).Pragma_Loc)));
         Write_Info_EOL;
      end loop;

      --  Loop through file table to output information for all units for which
      --  we have generated code, as marked by the Generate_Code flag.

      for Unit in Units.First .. Last_Unit loop
         if Units.Table (Unit).Generate_Code
           or else Unit = Main_Unit
         then
            Write_Info_EOL; -- blank line
            Write_Unit_Information (Unit);
         end if;
      end loop;

      Write_Info_EOL; -- blank line

      --  Output external version reference lines

      for J in 1 .. Version_Ref.Last loop
         Write_Info_Initiate ('E');
         Write_Info_Char (' ');

         for K in 1 .. String_Length (Version_Ref.Table (J)) loop
            Write_Info_Char_Code (Get_String_Char (Version_Ref.Table (J), K));
         end loop;

         Write_Info_EOL;
      end loop;

      --  Prepare to output the source dependency lines

      declare
         Unum : Unit_Number_Type;
         --  Number of unit being output

         Sind : Source_File_Index;
         --  Index of corresponding source file

         Fname : File_Name_Type;

      begin
         for J in 1 .. Num_Sdep loop
            Unum := Sdep_Table (J);
            Units.Table (Unum).Dependency_Num := J;
            Sind := Units.Table (Unum).Source_Index;

            Write_Info_Initiate ('D');
            Write_Info_Char (' ');

            --  Normal case of a unit entry with a source index

            if Sind /= No_Source_File then
               Fname := File_Name (Sind);

               --  Ensure that on platforms where the file names are not
               --  case sensitive, the recorded file name is in lower case.

               if not File_Names_Case_Sensitive then
                  Get_Name_String (Fname);
                  To_Lower (Name_Buffer (1 .. Name_Len));
                  Fname := Name_Find;
               end if;

               Write_Info_Name_May_Be_Quoted (Fname);
               Write_Info_Tab (25);
               Write_Info_Str (String (Time_Stamp (Sind)));
               Write_Info_Char (' ');
               Write_Info_Str (Get_Hex_String (Source_Checksum (Sind)));

               --  If subunit, add unit name, omitting the %b at the end

               if Present (Cunit (Unum))
                 and then Nkind (Unit (Cunit (Unum))) = N_Subunit
               then
                  Get_Decoded_Name_String (Unit_Name (Unum));
                  Write_Info_Char (' ');
                  Write_Info_Str (Name_Buffer (1 .. Name_Len - 2));
               end if;

               --  If Source_Reference pragma used output information

               if Num_SRef_Pragmas (Sind) > 0 then
                  Write_Info_Char (' ');

                  if Num_SRef_Pragmas (Sind) = 1 then
                     Write_Info_Nat (Int (First_Mapped_Line (Sind)));
                  else
                     Write_Info_Nat (0);
                  end if;

                  Write_Info_Char (':');
                  Write_Info_Name (Reference_Name (Sind));
               end if;

               --  Case where there is no source index (happens for missing
               --  files). In this case we write a dummy time stamp.

            else
               Write_Info_Name (Unit_File_Name (Unum));
               Write_Info_Tab (25);
               Write_Info_Str (String (Dummy_Time_Stamp));
               Write_Info_Char (' ');
               Write_Info_Str (Get_Hex_String (0));
            end if;

            Write_Info_EOL;
         end loop;
      end;

      --  Output cross-references

      if Opt.Xref_Active then
         Output_References;
      end if;

      --  Output SCO information if present

      if Generate_SCO then
         SCO_Output;
      end if;

      --  Output SPARK cross-reference information if needed

      if Opt.Xref_Active and then GNATprove_Mode then
         SPARK_Specific.Collect_SPARK_Xrefs (Sdep_Table => Sdep_Table,
                                             Num_Sdep   => Num_Sdep);
         SPARK_Specific.Output_SPARK_Xrefs;
      end if;

      --  Output final blank line and we are done. This final blank line is
      --  probably junk, but we don't feel like making an incompatible change!

      Write_Info_Terminate;
      Close_Output_Library_Info;
   end Write_ALI;

   ---------------------
   -- Write_Unit_Name --
   ---------------------

   procedure Write_Unit_Name (N : Node_Id) is
   begin
      if Nkind (N) = N_Identifier then
         Write_Info_Name (Chars (N));

      else
         pragma Assert (Nkind (N) = N_Selected_Component);
         Write_Unit_Name (Prefix (N));
         Write_Info_Char ('.');
         Write_Unit_Name (Selector_Name (N));
      end if;
   end Write_Unit_Name;

end Lib.Writ;
