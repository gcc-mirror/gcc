------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             L I B . W R I T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with ALI;            use ALI;
with Atree;          use Atree;
with Casing;         use Casing;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Fname;          use Fname;
with Fname.UF;       use Fname.UF;
with Lib.Util;       use Lib.Util;
with Lib.Xref;       use Lib.Xref;
with Nlists;         use Nlists;
with Gnatvsn;        use Gnatvsn;
with GNAT_CUDA;      use GNAT_CUDA;
with Opt;            use Opt;
with Osint;          use Osint;
with Osint.C;        use Osint.C;
with Output;         use Output;
with Par;
with Par_SCO;        use Par_SCO;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Stand;          use Stand;
with Scn;            use Scn;
with Sem_Eval;       use Sem_Eval;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;
with Uname;          use Uname;

with System.Case_Util; use System.Case_Util;
with System.WCh_Con;   use System.WCh_Con;

package body Lib.Writ is

   -----------------------
   -- Local subprograms --
   -----------------------

   function Present (N_Id : Name_Id) return Boolean;
   pragma Inline (Present);
   --  Determine whether a name with id N_Id exists

   procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id);
   pragma Inline (Write_Invocation_Construct);
   --  Write invocation construct IC_Id to the ALI file

   procedure Write_Invocation_Graph;
   pragma Inline (Write_Invocation_Graph);
   --  Write out the invocation graph

   procedure Write_Invocation_Graph_Attributes;
   pragma Inline (Write_Invocation_Graph_Attributes);
   --  Write out the attributes of the invocation graph

   procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id);
   pragma Inline (Write_Invocation_Relation);
   --  Write invocation relation IR_Id to the ALI file

   procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id);
   pragma Inline (Write_Invocation_Signature);
   --  Write invocation signature IS_Id to the ALI file

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
        (Unit_File_Name         => File_Name (S),
         Unit_Name              => No_Unit_Name,
         Expected_Unit          => No_Unit_Name,
         Source_Index           => S,
         Cunit                  => Empty,
         Cunit_Entity           => Empty,
         Dependency_Num         => 0,
         Dynamic_Elab           => False,
         Fatal_Error            => None,
         Generate_Code          => False,
         Has_RACW               => False,
         Filler                 => False,
         Ident_String           => Empty,
         Is_Predefined_Renaming => False,
         Is_Internal_Unit       => False,
         Is_Predefined_Unit     => False,
         Filler2                => False,
         Loading                => False,
         Main_Priority          => -1,
         Main_CPU               => -1,
         Munit_Index            => 0,
         No_Elab_Code_All       => False,
         Primary_Stack_Count    => 0,
         Sec_Stack_Count        => 0,
         Serial_Number          => 0,
         Version                => 0,
         Error_Location         => No_Location,
         OA_Setting             => 'O');
   end Add_Preprocessing_Dependency;

   ------------------------------
   -- Ensure_System_Dependency --
   ------------------------------

   procedure Ensure_System_Dependency is
      System_Uname : constant Unit_Name_Type :=
        Name_To_Unit_Name (Name_System);
      --  Unit name for system spec if needed for dummy entry

      System_Fname : File_Name_Type;
      --  File name for system spec if needed for dummy entry

   begin
      --  Nothing to do if we already compiled System

      if Is_Loaded (System_Uname) then
         return;
      end if;

      --  If no entry for system.ads in the units table, then add a entry
      --  to the units table for system.ads, which will be referenced when
      --  the ali file is generated. We need this because every unit depends
      --  on system as a result of Targparm scanning the system.ads file to
      --  determine the target dependent parameters for the compilation.

      System_Fname := File_Name (System_Source_File_Index);

      Units.Increment_Last;
      Units.Table (Units.Last) :=
        (Unit_File_Name         => System_Fname,
         Unit_Name              => System_Uname,
         Expected_Unit          => System_Uname,
         Source_Index           => System_Source_File_Index,
         Cunit                  => Empty,
         Cunit_Entity           => Empty,
         Dependency_Num         => 0,
         Dynamic_Elab           => False,
         Fatal_Error            => None,
         Generate_Code          => False,
         Has_RACW               => False,
         Filler                 => False,
         Ident_String           => Empty,
         Is_Predefined_Renaming => False,
         Is_Internal_Unit       => True,
         Is_Predefined_Unit     => True,
         Filler2                => False,
         Loading                => False,
         Main_Priority          => -1,
         Main_CPU               => -1,
         Munit_Index            => 0,
         No_Elab_Code_All       => False,
         Primary_Stack_Count    => 0,
         Sec_Stack_Count        => 0,
         Serial_Number          => 0,
         Version                => 0,
         Error_Location         => No_Location,
         OA_Setting             => 'O');
      Init_Unit_Name (Units.Last, System_Uname);

      --  Parse system.ads so that the checksum is set right. Style checks are
      --  not applied. The Ekind is set to ensure that this reference is always
      --  present in the ali file.

      declare
         Save_Mindex : constant Nat := Multiple_Unit_Index;
         Save_Style  : constant Boolean := Style_Check;
      begin
         Multiple_Unit_Index := 0;
         Style_Check := False;
         Initialize_Scanner (Units.Last, System_Source_File_Index);
         Discard_List (Par (Configuration_Pragmas => False));
         Mutate_Ekind (Cunit_Entity (Units.Last), E_Package);
         Set_Scope (Cunit_Entity (Units.Last), Standard_Standard);
         Style_Check := Save_Style;
         Multiple_Unit_Index := Save_Mindex;
      end;
   end Ensure_System_Dependency;

   -------------
   -- Present --
   -------------

   function Present (N_Id : Name_Id) return Boolean is
   begin
      return N_Id /= No_Name;
   end Present;

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
      Has_Implicit_With : array (Units.First .. Last_Unit) of Yes_No;
      --  Indicates if an implicit with has been given for the unit. Yes if
      --  certainly present, No if certainly absent, Unknown if not known.

      Sdep_Table : Unit_Ref_Table (1 .. Pos (Last_Unit - Units.First + 2));
      --  Sorted table of source dependencies. One extra entry in case we
      --  have to add a dummy entry for System.

      Num_Sdep : Nat := 0;
      --  Number of active entries in Sdep_Table

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Collect_Withs (Cunit : Node_Id);
      --  Collect with lines for entries in the context clause of the given
      --  compilation unit, Cunit.

      procedure Output_CUDA_Symbols (Unit_Num : Unit_Number_Type);
      --  Output CUDA symbols, so that the rest of the toolchain may know what
      --  symbols need registering with the CUDA runtime.

      procedure Write_Unit_Information (Unit_Num : Unit_Number_Type);
      --  Write out the library information for one unit for which code is
      --  generated (includes unit line and with lines).

      procedure Write_With_Lines;
      --  Write out with lines collected by calls to Collect_Withs

      -------------------
      -- Collect_Withs --
      -------------------

      procedure Collect_Withs (Cunit : Node_Id) is
         function Is_Implicit_With_Clause (Clause : Node_Id) return Boolean;
         pragma Inline (Is_Implicit_With_Clause);
         --  Determine whether a with clause denoted by Clause is implicit

         -----------------------------
         -- Is_Implicit_With_Clause --
         -----------------------------

         function Is_Implicit_With_Clause (Clause : Node_Id) return Boolean is
         begin
            --  With clauses created for ancestor units are marked as internal,
            --  however, they emulate the semantics in RM 10.1.2 (6/2), where
            --
            --    with A.B;
            --
            --  is almost equivalent to
            --
            --    with A;
            --    with A.B;
            --
            --  For ALI encoding purposes, they are considered to be explicit.
            --  Note that the clauses cannot be marked as explicit because they
            --  will be subjected to various checks related to with clauses and
            --  possibly cause false positives.

            if Parent_With (Clause) then
               return False;

            else
               return Is_Implicit_With (Clause);
            end if;
         end Is_Implicit_With_Clause;

         --  Local variables

         Item : Node_Id;
         Unum : Unit_Number_Type;

      --  Start of processing for Collect_Withs

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

               if Is_Implicit_With_Clause (Item) then

                  --  A previous explicit with clause withs the unit. Retain
                  --  this classification, as it reflects the source relations
                  --  between units.

                  if Has_Implicit_With (Unum) = No then
                     null;

                  --  Otherwise this is either the first time any clause withs
                  --  the unit, or the unit is already implicitly withed.

                  else
                     Has_Implicit_With (Unum) := Yes;
                  end if;

               --  Otherwise the current with clause is explicit. Such clauses
               --  take precedence over existing implicit clauses because they
               --  reflect the source relations between unit.

               else
                  Has_Implicit_With (Unum) := No;
               end if;
            end if;

            Next (Item);
         end loop;
      end Collect_Withs;

      -------------------------
      -- Output_CUDA_Symbols --
      -------------------------

      procedure Output_CUDA_Symbols (Unit_Num : Unit_Number_Type) is
         Unit_Id     : constant Node_Id := Unit (Cunit (Unit_Num));
         Spec_Id     : Node_Id;
         Kernels     : Elist_Id;
         Kernel_Elm  : Elmt_Id;
         Kernel      : Entity_Id;
      begin
         if not Enable_CUDA_Expansion
           or else Nkind (Unit_Id) = N_Null_Statement
         then
            return;
         end if;
         Spec_Id := (if Nkind (Unit_Id) = N_Package_Body
           then Corresponding_Spec (Unit_Id)
           else Defining_Unit_Name (Specification (Unit_Id)));
         Kernels := Get_CUDA_Kernels (Spec_Id);
         if No (Kernels) then
            return;
         end if;

         Kernel_Elm := First_Elmt (Kernels);
         while Present (Kernel_Elm) loop
            Kernel := Node (Kernel_Elm);

            Write_Info_Initiate ('K');
            Write_Info_Char (' ');
            Write_Info_Name (Chars (Kernel));
            Write_Info_Terminate;
            Next_Elmt (Kernel_Elm);
         end loop;

      end Output_CUDA_Symbols;

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
              or else Ekind (Uent) = E_Package
              or else Is_Generic_Unit (Uent))

            --  and an elaboration entity was declared ...

            and then Present (Elaboration_Entity (Uent))

            --  and either the elaboration flag is required ...

            and then (Elaboration_Entity_Required (Uent)

               --  or this unit has elaboration code ...

               or else not Has_No_Elaboration_Code (Unode)

               --  or this unit has a separate body and this
               --  body has elaboration code.

               or else
                 (Ekind (Uent) = E_Package
                   and then Present (Body_Entity (Uent))
                   and then
                     not Has_No_Elaboration_Code
                           (Parent (Declaration_Node (Body_Entity (Uent))))))
         then
            Write_Info_Str (" EE");
         end if;

         if Has_No_Elaboration_Code (Unode) then
            Write_Info_Str (" NE");
         end if;

         Write_Info_Str (" O");
         Write_Info_Char (OA_Setting (Unit_Num));

         --  For a package instance with a body that is a library unit, the two
         --  compilation units share Cunit_Entity so we cannot rely on Uent.

         if Ukind in N_Package_Declaration | N_Package_Body then
            declare
               E : constant Entity_Id := Defining_Entity (Unit (Unode));

            begin
               if Ekind (E) in E_Package | E_Package_Body
                 and then Present (Finalizer (E))
               then
                  Write_Info_Str (" PF");
               end if;
            end;
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

         if Serious_Errors_Detected /= 0 then
            Write_Info_Str (" SE");
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

         if not Is_Internal_Unit (Unit_Num) then
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
            Has_Implicit_With  (J) := Unknown;
         end loop;

         Collect_Withs (Unode);

         --  For a body, we must also check for any subunits which belong to
         --  it and which have context clauses of their own, since these
         --  with'ed units are part of its own elaboration dependencies.

         if Nkind (Unit (Unode)) in N_Unit_Body then
            for S in Units.First .. Last_Unit loop

               --  We are only interested in subunits. For preproc. data and
               --  def. files, Cunit is Empty, so we need to test that first.

               if Cunit (S) /= Empty
                 and then Nkind (Unit (Cunit (S))) = N_Subunit
               then
                  Pnode := Library_Unit (Cunit (S));

                  --  In gnatc mode, the errors in the subunits will not have
                  --  been recorded, but the analysis of the subunit may have
                  --  failed. There is no information to add to ALI file in
                  --  this case.

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

         --  Generate task stack lines

         if Primary_Stack_Count (Unit_Num) > 0
           or else Sec_Stack_Count (Unit_Num) > 0
         then
            Write_Info_Initiate ('T');
            Write_Info_Char (' ');
            Write_Info_Int (Primary_Stack_Count (Unit_Num));
            Write_Info_Char (' ');
            Write_Info_Int (Sec_Stack_Count (Unit_Num));
            Write_Info_EOL;
         end if;

         --  Generate the linker option lines

         for J in 1 .. Linker_Option_Lines.Last loop

            --  Pragma Linker_Options is not allowed in predefined generic
            --  units. This is because they won't be read, due to the fact that
            --  with lines for generic units lack the file name and lib name
            --  parameters (see Lib_Writ spec for an explanation).

            if Is_Generic_Unit (Cunit_Entity (Main_Unit))
              and then Is_Predefined_Unit (Current_Sem_Unit)
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
               N : constant Node_Id          := Notes.Table (J);
               L : constant Source_Ptr       := Sloc (N);
               U : constant Unit_Number_Type :=
                     Unit (Get_Source_File_Index (L));
               C : Character;

               Note_Unit : Unit_Number_Type;
               --  The unit in whose U section this note must be emitted:
               --  notes for subunits are emitted along with the main unit;
               --  all other notes are emitted as part of the enclosing
               --  compilation unit.

            begin
               if U /= No_Unit and then Nkind (Unit (Cunit (U))) = N_Subunit
               then
                  Note_Unit := Main_Unit;
               else
                  Note_Unit := U;
               end if;

               --  No action needed for pragmas removed by the expander (for
               --  example, pragmas of ignored ghost entities).

               if Nkind (N) = N_Null_Statement then
                  pragma Assert (Nkind (Original_Node (N)) = N_Pragma);
                  null;

               elsif Note_Unit = Unit_Num then
                  Write_Info_Initiate ('N');
                  Write_Info_Char (' ');

                  case Pragma_Name (N) is
                     when Name_Annotate | Name_GNAT_Annotate =>
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

                  --  Indicate source file of annotation if different from
                  --  compilation unit source file (case of annotation coming
                  --  from a separate).

                  if Get_Source_File_Index (L) /= Source_Index (Unit_Num) then
                     Write_Info_Char (':');
                     Write_Info_Name (File_Name (Get_Source_File_Index (L)));
                  end if;

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
                             and then Is_OK_Static_Expression (Expr)
                           then
                              Write_Info_Uint (Intval (Expr));

                           elsif Nkind (Expr) = N_String_Literal
                             and then Is_OK_Static_Expression (Expr)
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
         Pname      : constant Unit_Name_Type :=
                        Get_Parent_Spec_Name (Unit_Name (Main_Unit));
         Body_Fname : File_Name_Type;
         Body_Index : Nat;
         Cunit      : Node_Id;
         Fname      : File_Name_Type;
         Num_Withs  : Int := 0;
         Unum       : Unit_Number_Type;
         Uname      : Unit_Name_Type;
         With_Table : Unit_Ref_Table (1 .. Pos (Last_Unit - Units.First + 1));

         procedure Write_With_File_Names
           (Nam : in out File_Name_Type;
            Idx : Nat);
         --  Write source file name Nam and ALI file name for unit index Idx.
         --  Possibly change Nam to lowercase (generating a new file name).

         ---------------------------
         -- Write_With_File_Names --
         ---------------------------

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
            --  but is required by the binder for elaboration purposes. For
            --  preprocessing data and definition files, there is no Unit_Name,
            --  check for that first.

            if Present (Unit_Name (J))
              and then (With_Flags (J) or else Unit_Name (J) = Pname)
            then
               Num_Withs := Num_Withs + 1;
               With_Table (Num_Withs) := J;
            end if;
         end loop;

         --  Sort and output the table

         Sort (With_Table (1 .. Num_Withs));

         for J in 1 .. Num_Withs loop
            Unum := With_Table (J);

            --  Do not generate a with line for an ignored Ghost unit because
            --  the unit does not have an ALI file.

            if Is_Ignored_Ghost_Entity (Cunit_Entity (Unum)) then
               goto Next_With_Line;
            end if;

            Cunit := Units.Table (Unum).Cunit;
            Uname := Units.Table (Unum).Unit_Name;
            Fname := Units.Table (Unum).Unit_File_Name;

            --  Limited with clauses must be processed first because they are
            --  the most specific among the three kinds.

            if Ekind (Cunit_Entity (Unum)) = E_Package
              and then From_Limited_With (Cunit_Entity (Unum))
            then
               Write_Info_Initiate ('Y');

            elsif Has_Implicit_With (Unum) = Yes then
               Write_Info_Initiate ('Z');

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
                    and then Generic_May_Lack_ALI (Unum))

              --  In SPARK mode, always generate the dependencies on ALI
              --  files, which are required to compute frame conditions
              --  of subprograms.

              or else GNATprove_Mode
            then
               Write_Info_Tab (25);

               if Is_Spec_Name (Uname) then
                  Body_Fname :=
                    Get_File_Name
                      (Uname    => Get_Body_Name (Uname),
                       Subunit  => False,
                       May_Fail => True);

                  Body_Index := Get_Unit_Index (Get_Body_Name (Uname));

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

         <<Next_With_Line>>
            null;
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
                     goto Next_Restriction_Set;
                  end if;
               end loop;

               --  Otherwise generate the entry

               Write_Info_Initiate ('W');
               Write_Info_Char (' ');
               Write_Info_Name (Unam);
               Write_Info_EOL;

            <<Next_Restriction_Set>>
               null;
            end loop;
         end;
      end Write_With_Lines;

   --  Start of processing for Write_ALI

   begin
      --  We never write an ALI file if the original operating mode was
      --  syntax-only (-gnats switch used in compiler invocation line).

      if Original_Operating_Mode = Check_Syntax then
         return;
      end if;

      --  Generation of ALI files may be disabled, e.g. for formal verification
      --  back-end.

      if Disable_ALI_File then
         return;
      end if;

      --  Build sorted source dependency table

      for Unum in Units.First .. Last_Unit loop
         if Cunit_Entity (Unum) = Empty
           or else not From_Limited_With (Cunit_Entity (Unum))
         then
            --  Units that are not analyzed need not appear in the dependency
            --  list. These units are either units appearing in limited_with
            --  clauses of other units, or units loaded for inlining that end
            --  up not inlined by a later decision of the inlining code, to
            --  prevent circularities. We want to exclude these files from the
            --  list of dependencies, so that the dependency number of other
            --  is correctly set, as that number is used by cross-reference
            --  tools to relate entity information to the unit in which they
            --  are declared.

            if Present (Cunit_Entity (Unum))
              and then Ekind (Cunit_Entity (Unum)) = E_Void
              and then Nkind (Unit (Cunit (Unum))) /= N_Subunit
              and then Serious_Errors_Detected = 0
            then
               null;

            else
               Num_Sdep := Num_Sdep + 1;
               Sdep_Table (Num_Sdep) := Unum;
            end if;
         end if;
      end loop;

      --  Sort the table so that the D lines are in order

      Lib.Sort (Sdep_Table (1 .. Num_Sdep));

      --  Acquire compilation arguments and prepare to write out a new ali
      --  file.

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
              and then Is_Generic_Subprogram (Corresponding_Spec (U))
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

      --  Output CUDA Kernel lines

      for Unit in Units.First .. Last_Unit loop
         if Present (Cunit (Unit)) then
            Output_CUDA_Symbols (Unit);
         end if;
      end loop;

      --  Output parameters ('P') line

      Write_Info_Initiate ('P');

      if Compilation_Errors then
         Write_Info_Str (" CE");
      end if;

      if Opt.Detect_Blocking then
         Write_Info_Str (" DB");
      end if;

      if Tasking_Used and then not Is_Predefined_Unit (Main_Unit) then
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

      if GNATprove_Mode then
         Write_Info_Str (" GP");
      end if;

      if Partition_Elaboration_Policy /= ' ' then
         Write_Info_Str  (" E");
         Write_Info_Char (Partition_Elaboration_Policy);
      end if;

      if Opt.Interrupts_System_By_Default then
         Write_Info_Str (" ID");
      end if;

      if No_Component_Reordering_Config then
         Write_Info_Str (" NC");
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

      if Default_SSO_Config /= ' ' then
         Write_Info_Str (" O");
         Write_Info_Char (Default_SSO_Config);
      end if;

      if Sec_Stack_Used then
         Write_Info_Str (" SS");
      end if;

      if Unreserve_All_Interrupts then
         Write_Info_Str (" UA");
      end if;

      if ZCX_Exceptions then
         Write_Info_Str (" ZX");
      end if;

      Write_Info_EOL;

      --  Before outputting the restrictions line, update the setting of
      --  the No_Elaboration_Code flag. Violations of this restriction
      --  cannot be detected until after the backend has been called since
      --  it is the backend that sets this flag. We have to check all units
      --  for which we have generated code

      for Unit in Units.First .. Last_Unit loop
         if Units.Table (Unit).Generate_Code then
            if not Has_No_Elaboration_Code (Cunit (Unit)) then
               Main_Restrictions.Violated (No_Elaboration_Code) := True;
               exit;
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
            Sind := Source_Index (Unum);

            Write_Info_Initiate ('D');
            Write_Info_Char (' ');

            --  Normal case of a unit entry with a source index

            if Sind > No_Source_File then

               if Config_Files_Store_Basename then
                  Fname := Strip_Directory (File_Name (Sind));
               else
                  Fname := File_Name (Sind);
               end if;

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

               --  If the dependency comes from a limited_with clause, record
               --  limited_checksum. This is disabled until full checksum
               --  changes are checked.

               --  if Present (Cunit_Entity (Unum))
               --    and then From_Limited_With (Cunit_Entity (Unum))
               --  then
               --     Write_Info_Char (' ');
               --     Write_Info_Char ('Y');
               --     Write_Info_Str (Get_Hex_String (Limited_Chk_Sum (Sind)));
               --  end if;

               --  If subunit, add unit name, omitting the %b at the end

               if Present (Cunit (Unum)) then
                  Get_Decoded_Name_String (Unit_Name (Unum));
                  Write_Info_Char (' ');

                  if Nkind (Unit (Cunit (Unum))) = N_Subunit then
                     Write_Info_Str (Name_Buffer (1 .. Name_Len - 2));
                  else
                     Write_Info_Str (Name_Buffer (1 .. Name_Len));
                  end if;
               end if;

               --  If Source_Reference pragma used, output information

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

      --  Output the invocation graph

      Write_Invocation_Graph;

      --  Output cross-references

      if Opt.Xref_Active then
         Output_References;
      end if;

      --  Output SCO information if present

      if Generate_SCO then
         SCO_Record_Filtered;
         SCO_Output;
      end if;

      --  Output final blank line and we are done. This final blank line is
      --  probably junk, but we don't feel like making an incompatible change.

      Write_Info_Terminate;
      Close_Output_Library_Info;
   end Write_ALI;

   --------------------------------
   -- Write_Invocation_Construct --
   --------------------------------

   procedure Write_Invocation_Construct (IC_Id : Invocation_Construct_Id) is
   begin
      --  G header

      Write_Info_Initiate ('G');
      Write_Info_Char     (' ');

      --  line-kind

      Write_Info_Char
        (Invocation_Graph_Line_Kind_To_Code (Invocation_Construct_Line));
      Write_Info_Char (' ');

      --  construct-kind

      Write_Info_Char (Invocation_Construct_Kind_To_Code (Kind (IC_Id)));
      Write_Info_Char (' ');

      --  construct-spec-placement

      Write_Info_Char
        (Declaration_Placement_Kind_To_Code (Spec_Placement (IC_Id)));
      Write_Info_Char (' ');

      --  construct-body-placement

      Write_Info_Char
        (Declaration_Placement_Kind_To_Code (Body_Placement (IC_Id)));
      Write_Info_Char (' ');

      --  construct-signature

      Write_Invocation_Signature (Signature (IC_Id));
      Write_Info_EOL;
   end Write_Invocation_Construct;

   ---------------------------------------
   -- Write_Invocation_Graph_Attributes --
   ---------------------------------------

   procedure Write_Invocation_Graph_Attributes is
   begin
      --  G header

      Write_Info_Initiate ('G');
      Write_Info_Char     (' ');

      --  line-kind

      Write_Info_Char
        (Invocation_Graph_Line_Kind_To_Code
          (Invocation_Graph_Attributes_Line));
      Write_Info_Char (' ');

      --  encoding-kind

      Write_Info_Char
        (Invocation_Graph_Encoding_Kind_To_Code (Invocation_Graph_Encoding));
      Write_Info_EOL;
   end Write_Invocation_Graph_Attributes;

   ----------------------------
   -- Write_Invocation_Graph --
   ----------------------------

   procedure Write_Invocation_Graph is
   begin
      Write_Invocation_Graph_Attributes;

      --  First write out all invocation constructs declared within the current
      --  unit. This ensures that when this invocation is read, the invocation
      --  constructs are materialized before they are referenced by invocation
      --  relations.

      For_Each_Invocation_Construct (Write_Invocation_Construct'Access);

      --  Write out all invocation relations that originate from invocation
      --  constructs delared in the current unit.

      For_Each_Invocation_Relation (Write_Invocation_Relation'Access);
   end Write_Invocation_Graph;

   -------------------------------
   -- Write_Invocation_Relation --
   -------------------------------

   procedure Write_Invocation_Relation (IR_Id : Invocation_Relation_Id) is
   begin
      --  G header

      Write_Info_Initiate ('G');
      Write_Info_Char     (' ');

      --  line-kind

      Write_Info_Char
        (Invocation_Graph_Line_Kind_To_Code (Invocation_Relation_Line));
      Write_Info_Char (' ');

      --  relation-kind

      Write_Info_Char (Invocation_Kind_To_Code (Kind (IR_Id)));
      Write_Info_Char (' ');

      --  (extra-name | "none")

      if Present (Extra (IR_Id)) then
         Write_Info_Name (Extra (IR_Id));
      else
         Write_Info_Str ("none");
      end if;

      Write_Info_Char (' ');

      --  invoker-signature

      Write_Invocation_Signature (Invoker (IR_Id));
      Write_Info_Char (' ');

      --  target-signature

      Write_Invocation_Signature (Target (IR_Id));

      Write_Info_EOL;
   end Write_Invocation_Relation;

   --------------------------------
   -- Write_Invocation_Signature --
   --------------------------------

   procedure Write_Invocation_Signature (IS_Id : Invocation_Signature_Id) is
   begin
      --  [

      Write_Info_Char ('[');

      --  name

      Write_Info_Name (Name (IS_Id));
      Write_Info_Char (' ');

      --  scope

      Write_Info_Name (IS_Scope (IS_Id));
      Write_Info_Char (' ');

      --  line

      Write_Info_Nat  (Line (IS_Id));
      Write_Info_Char (' ');

      --  column

      Write_Info_Nat  (Column (IS_Id));
      Write_Info_Char (' ');

      --  (locations | "none")

      if Present (Locations (IS_Id)) then
         Write_Info_Name (Locations (IS_Id));
      else
         Write_Info_Str ("none");
      end if;

      --  ]

      Write_Info_Char (']');
   end Write_Invocation_Signature;

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
