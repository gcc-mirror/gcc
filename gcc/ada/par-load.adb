------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             P A R . L O A D                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  The Par.Load procedure loads all units that are definitely required before
--  it makes any sense at all to proceed with semantic analysis, including
--  with'ed units, corresponding specs for bodies, parents of child specs,
--  and parents of subunits. All these units are loaded and pointers installed
--  in the tree as described in the spec of package Lib.

with Fname.UF; use Fname.UF;
with Lib.Load; use Lib.Load;
with Namet.Sp; use Namet.Sp;
with Uname;    use Uname;
with Osint;    use Osint;
with Sinput.L; use Sinput.L;
with Stylesw;  use Stylesw;

with GNAT.Spelling_Checker; use GNAT.Spelling_Checker;

separate (Par)
procedure Load is

   File_Name : File_Name_Type;
   --  Name of file for current unit, derived from unit name

   Cur_Unum : constant Unit_Number_Type := Current_Source_Unit;
   --  Unit number of unit that we just finished parsing. Note that we need
   --  to capture this, because Source_Unit will change as we parse new
   --  source files in the multiple main source file case.

   Curunit : constant Node_Id := Cunit (Cur_Unum);
   --  Compilation unit node for current compilation unit

   Loc : Source_Ptr := Sloc (Curunit);
   --  Source location for compilation unit node

   Save_Style_Check  : Boolean;
   Save_Style_Checks : Style_Check_Options;
   --  Save style check so it can be restored later

   With_Cunit : Node_Id;
   --  Compilation unit node for withed unit

   Context_Node : Node_Id;
   --  Next node in context items list

   With_Node : Node_Id;
   --  N_With_Clause node

   Spec_Name : Unit_Name_Type;
   --  Unit name of required spec

   Body_Name : Unit_Name_Type;
   --  Unit name of corresponding body

   Unum : Unit_Number_Type;
   --  Unit number of loaded unit

   Limited_With_Found : Boolean := False;
   --  We load the context items in two rounds: the first round handles normal
   --  withed units and the second round handles Ada 2005 limited-withed units.
   --  This is required to allow the low-level circuitry that detects circular
   --  dependencies of units the correct notification of errors (see comment
   --  below). This variable is used to indicate that the second round is
   --  required.

   function Same_File_Name_Except_For_Case
     (Expected_File_Name : File_Name_Type;
      Actual_File_Name   : File_Name_Type) return Boolean;
   --  Given an actual file name and an expected file name (the latter being
   --  derived from the unit name), determine if they are the same except for
   --  possibly different casing of letters.

   ------------------------------------
   -- Same_File_Name_Except_For_Case --
   ------------------------------------

   function Same_File_Name_Except_For_Case
     (Expected_File_Name : File_Name_Type;
      Actual_File_Name   : File_Name_Type) return Boolean
   is
   begin
      Get_Name_String (Actual_File_Name);
      Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

      declare
         Lower_Case_Actual_File_Name : String (1 .. Name_Len);

      begin
         Lower_Case_Actual_File_Name := Name_Buffer (1 .. Name_Len);
         Get_Name_String (Expected_File_Name);
         Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
         return Lower_Case_Actual_File_Name = Name_Buffer (1 .. Name_Len);
      end;

   end Same_File_Name_Except_For_Case;

   function Actual_Name_Collides_With_Predefined_Unit_Name
     (Actual_File_Name : File_Name_Type) return Boolean;
   --  Given an actual file name, determine if it is a child unit whose parent
   --  unit is a single letter that is a, g, i, or s. Such a name could create
   --  confusion with predefined units and thus is required to use a tilde
   --  instead of the minus as the second character.

   ----------------------------------------------------
   -- Actual_Name_Collides_With_Predefined_Unit_Name --
   ----------------------------------------------------

   function Actual_Name_Collides_With_Predefined_Unit_Name
     (Actual_File_Name : File_Name_Type) return Boolean
   is
      N1 : Character;
   begin
      Get_Name_String (Actual_File_Name);
      N1 := Name_Buffer (1);
      return Name_Len > 1
        and then Name_Buffer (2) = '-'
        and then (N1 = 'a' or else N1 = 'g' or else N1 = 'i' or else N1 = 's');
   end Actual_Name_Collides_With_Predefined_Unit_Name;

--  Start of processing for Load

begin
   --  Don't do any loads if we already had a fatal error

   if Fatal_Error (Cur_Unum) = Error_Detected then
      return;
   end if;

   Save_Style_Check_Options (Save_Style_Checks);
   Save_Style_Check := Opt.Style_Check;

   --  If main unit, set Main_Unit_Entity (this will get overwritten if the
   --  main unit has a separate spec, that happens later on in Load).

   if Cur_Unum = Main_Unit then
      Main_Unit_Entity := Cunit_Entity (Main_Unit);
   end if;

   --  If we have no unit name, things are seriously messed up by previous
   --  errors, and we should not try to continue compilation.

   if Unit_Name (Cur_Unum) = No_Unit_Name then
      raise Unrecoverable_Error;
   end if;

   --  Next step, make sure that the unit name matches the file name
   --  and issue a warning message if not. We only output this for the
   --  main unit, since for other units it is more serious and is
   --  caught in a separate test below. We also inhibit the message in
   --  multiple unit per file mode, because in this case the relation
   --  between file name and unit name is broken.

   File_Name :=
     Get_File_Name
       (Unit_Name (Cur_Unum),
        Subunit => Nkind (Unit (Cunit (Cur_Unum))) = N_Subunit);

   if Cur_Unum = Main_Unit
     and then Multiple_Unit_Index = 0
     and then File_Name /= Unit_File_Name (Cur_Unum)
     and then (File_Names_Case_Sensitive
                or not Same_File_Name_Except_For_Case
                         (File_Name, Unit_File_Name (Cur_Unum)))
   then
      Error_Msg_File_1 := File_Name;
      if Actual_Name_Collides_With_Predefined_Unit_Name
           (Unit_File_Name (Cur_Unum))
      then
         Error_Msg
           ("??file name may conflict with predefined units, "
            & "the expected file name for this unit is{",
            Sloc (Curunit));
      else
         Error_Msg
           ("??file name does not match unit name, should be{",
            Sloc (Curunit));
      end if;
   end if;

   --  For units other than the main unit, the expected unit name is set and
   --  must be the same as the actual unit name, or we are in big trouble, and
   --  abandon the compilation since there are situations where this really
   --  gets us into bad trouble (e.g. some subunit situations).

   if Cur_Unum /= Main_Unit
     and then Expected_Unit (Cur_Unum) /= Unit_Name (Cur_Unum)
   then
      Loc := Error_Location (Cur_Unum);
      Error_Msg_File_1 := Unit_File_Name (Cur_Unum);
      Get_Name_String (Error_Msg_File_1);

      --  Check for predefined file case

      if Name_Len > 1
        and then Name_Buffer (1) in 'a' | 's' | 'i' | 'g'
        and then Name_Buffer (2) = '-'
      then
         declare
            Expect_Name : constant Unit_Name_Type := Expected_Unit (Cur_Unum);
            Actual_Name : constant Unit_Name_Type := Unit_Name (Cur_Unum);

         begin
            Error_Msg_Unit_1 := Expect_Name;
            Error_Msg -- CODEFIX
              ("$$ is not a predefined library unit!", Loc);

            --  In the predefined file case, we know the user did not
            --  construct their own package, but we got the wrong one.
            --  This means that the name supplied by the user crunched
            --  to something we recognized, but then the file did not
            --  contain the unit expected. Most likely this is due to
            --  a misspelling, e.g.

            --    with Ada.Calender;

            --  This crunches to a-calend, which indeed contains the unit
            --  Ada.Calendar, and we can diagnose the misspelling. This
            --  is a simple heuristic, but it catches many common cases
            --  of misspelling of predefined unit names without needing
            --  a full list of them.

            --  Before actually issuing the message, we will check that the
            --  unit name is indeed a plausible misspelling of the one we got.

            if Is_Bad_Spelling_Of
              (Name_Id (Expect_Name), Name_Id (Actual_Name))
            then
               Error_Msg_Unit_1 := Actual_Name;
               Error_Msg -- CODEFIX
                 ("possible misspelling of $$!", Loc);
            end if;
         end;

      --  Non-predefined file name case. In this case we generate a message
      --  and then we quit, because we are in big trouble, and if we try
      --  to continue compilation, we get into some nasty situations
      --  (for example in some subunit cases).

      else
         Error_Msg ("file { does not contain expected unit!", Loc);
         Error_Msg_Unit_1 := Expected_Unit (Cur_Unum);
         Error_Msg ("\\expected unit $!", Loc);
         Error_Msg_Unit_1 := Unit_Name (Cur_Unum);
         Error_Msg ("\\found unit $!", Loc);
      end if;

      --  In both cases, flag the fatal error and give up

      Set_Fatal_Error (Cur_Unum, Error_Detected);
      return;
   end if;

   --  If current unit is a body, load its corresponding spec

   if Nkind (Unit (Curunit)) in N_Package_Body | N_Subprogram_Body then
      Spec_Name := Get_Spec_Name (Unit_Name (Cur_Unum));
      Unum :=
        Load_Unit
          (Load_Name  => Spec_Name,
           Required   => False,
           Subunit    => False,
           Error_Node => Curunit,
           Corr_Body  => Cur_Unum,
           PMES       => (Cur_Unum = Main_Unit));

      --  If we successfully load the unit, then set the spec/body pointers.
      --  Once again note that if the loaded unit has a fatal error, Load will
      --  have set our Fatal_Error flag to propagate this condition.

      if Unum /= No_Unit then
         Set_Spec_Lib_Unit (Curunit, Cunit (Unum));
         Set_Body_Lib_Unit (Cunit (Unum), Curunit);

         --  If this is a separate spec for the main unit, then we reset
         --  Main_Unit_Entity to point to the entity for this separate spec
         --  and this is also where we generate the SCO's for this spec.

         if Cur_Unum = Main_Unit then

            --  We generate code for the main unit body, so we need to generate
            --  code for its spec too.

            Set_Generate_Code (Unum, True);

            Main_Unit_Entity := Cunit_Entity (Unum);

            if Generate_SCO then
               SCO_Record_Raw (Unum);
            end if;
         end if;

      --  If we don't find the spec, then if we have a subprogram body, we
      --  are still OK, we just have a case of a body acting as its own spec

      elsif Nkind (Unit (Curunit)) = N_Subprogram_Body then
         Set_Acts_As_Spec (Curunit, True);
         Set_Spec_Lib_Unit (Curunit, Curunit);

      --  Otherwise we do have an error, repeat the load request for the spec
      --  with Required set True to generate an appropriate error message.

      else
         Unum :=
           Load_Unit
             (Load_Name  => Spec_Name,
              Required   => True,
              Subunit    => False,
              Error_Node => Curunit);
         return;
      end if;

   --  If current unit is a child unit spec, load its parent. If the child unit
   --  is loaded through a limited with, the parent must be as well.

   elsif Nkind (Unit (Curunit)) in N_Package_Declaration
                                 | N_Subprogram_Declaration
                                 | N_Generic_Declaration
                                 | N_Generic_Instantiation
                                 | N_Renaming_Declaration
   then
      --  Turn style checks off for parent unit

      if not GNAT_Mode then
         Reset_Style_Check_Options;
      end if;

      Spec_Name := Get_Parent_Spec_Name (Unit_Name (Cur_Unum));

      if Present (Spec_Name) then
         Unum :=
           Load_Unit
             (Load_Name  => Spec_Name,
              Required   => True,
              Subunit    => False,
              Error_Node => Curunit);

         if Unum /= No_Unit then
            Set_Parent_Spec (Unit (Curunit), Cunit (Unum));
         end if;
      end if;

   --  If current unit is a subunit, then load its parent body

   elsif Nkind (Unit (Curunit)) = N_Subunit then
      Body_Name := Get_Parent_Body_Name (Unit_Name (Cur_Unum));
      Unum :=
        Load_Unit
          (Load_Name  => Body_Name,
           Required   => True,
           Subunit    => False,
           Error_Node => Name (Unit (Curunit)));

      if Unum /= No_Unit then
         Set_Subunit_Parent (Curunit, Cunit (Unum));
      end if;
   end if;

   --  Now we load with'ed units, with style checks turned off

   if not GNAT_Mode then
      Reset_Style_Check_Options;
   end if;

   --  Load the context items in two rounds: the first round handles normal
   --  withed units and the second round handles Ada 2005 limited-withed units.
   --  This is required to allow the low-level circuitry that detects circular
   --  dependencies of units the correct notification of the following error:

   --       limited with D;
   --       with D;                  with C;
   --       package C is ...         package D is ...

   for Round in 1 .. 2 loop
      Context_Node := First (Context_Items (Curunit));
      while Present (Context_Node) loop

         --  During the first round we check if there is some limited-with
         --  context clause; otherwise the second round will be skipped

         if Nkind (Context_Node) = N_With_Clause
           and then Round = 1
           and then Limited_Present (Context_Node)
         then
            Limited_With_Found := True;
         end if;

         if Nkind (Context_Node) = N_With_Clause
           and then ((Round = 1 and then not Limited_Present (Context_Node))
                        or else
                     (Round = 2 and then Limited_Present (Context_Node)))
         then
            With_Node := Context_Node;
            Spec_Name := Get_Unit_Name (With_Node);

            Unum :=
              Load_Unit
                (Load_Name  => Spec_Name,
                 Required   => False,
                 Subunit    => False,
                 Error_Node => With_Node,
                 Renamings  => True,
                 With_Node  => Context_Node);

            --  If we find the unit, then set spec pointer in the N_With_Clause
            --  to point to the compilation unit for the spec. Remember that
            --  the Load routine itself sets our Fatal_Error flag if the loaded
            --  unit gets a fatal error, so we don't need to worry about that.

            if Unum /= No_Unit then
               Set_Withed_Lib_Unit (With_Node, Cunit (Unum));

            --  If the spec isn't found, then try finding the corresponding
            --  body, since it is possible that we have a subprogram body
            --  that is acting as a spec (since no spec is present).

            else
               Body_Name := Get_Body_Name (Spec_Name);
               Unum :=
                 Load_Unit
                   (Load_Name  => Body_Name,
                    Required   => False,
                    Subunit    => False,
                    Error_Node => With_Node,
                    Renamings  => True);

               --  If we got a subprogram body, then mark that we are using
               --  the body as a spec in the file table, and set
               --  Withed_Lib_Unit of the N_With_Clause to point to
               --  the body entity.

               if Unum /= No_Unit
                 and then Nkind (Unit (Cunit (Unum))) = N_Subprogram_Body
               then
                  With_Cunit := Cunit (Unum);
                  Set_Withed_Lib_Unit (With_Node, With_Cunit);

                  --  If we have errors, Acts_As_Spec and Spec_Lib_Unit might
                  --  not be set; set them for better error recovery.

                  if Serious_Errors_Detected > 0 then
                     Set_Acts_As_Spec (With_Cunit, True);
                     Set_Spec_Lib_Unit (With_Cunit, With_Cunit);

                  --  Otherwise, these field should already by set

                  else
                     pragma Assert (Acts_As_Spec (With_Cunit));
                     pragma Assert (Spec_Lib_Unit (With_Cunit) = With_Cunit);
                  end if;

               --  If we couldn't find the body, or if it wasn't a body spec
               --  then we are in trouble. We make one more call to Load to
               --  require the spec. We know it will fail of course, the
               --  purpose is to generate the required error message (we prefer
               --  that this message refer to the missing spec, not the body)

               else
                  Unum :=
                    Load_Unit
                      (Load_Name  => Spec_Name,
                       Required   => True,
                       Subunit    => False,
                       Error_Node => With_Node,
                       Renamings  => True);

                  --  Here we create a dummy package unit for the missing unit

                  Unum := Create_Dummy_Package_Unit (With_Node, Spec_Name);
                  Set_Withed_Lib_Unit (With_Node, Cunit (Unum));
               end if;
            end if;
         end if;

         Next (Context_Node);
      end loop;

      exit when not Limited_With_Found;
   end loop;

   --  Restore style/validity check mode for main unit

   Set_Style_Check_Options (Save_Style_Checks);
   Opt.Style_Check := Save_Style_Check;
end Load;
