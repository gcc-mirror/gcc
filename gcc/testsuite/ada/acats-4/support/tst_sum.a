-- TST_SUM.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--
--*
--
-- PURPOSE:
--      This package defines the data structures that represent a summary of
--      each test file in the ACATS. This, combined with an test run event
--      trace, can then be used to grade the results of an ACATS test run.
--
-- CHANGE HISTORY:
--      04 Mar 2016   RLB  Created package.
--      16 May 2016   RLB  Added Annex_C_Requirement.

with Trace, Ada.Text_IO;
package Test_Summary is

   -- The basic idea is that each test file is summarized by a (usually)
   -- small number of records, which collectively describe everything
   -- interesting (from the perspective of test grading) about the test file.
   --
   -- These test records then can be used to determine whether or not a
   -- particular event trace represents passing or failing behavior for
   -- the test represented by the test records.

   type Info_Kind_Type is (Unknown,
        Compilation_Unit,             -- A compilation unit
        Error,                        -- An ERROR: tag in the file.
        Possible_Error,               -- A POSSIBLE ERROR: tag in the file.
        Optional_Error,               -- An OPTIONAL ERROR: tag in the file.
        NA_Error,                     -- A N/A => ERROR tag in the file.
        Annex_C_Requirement,          -- An ANX-C RQMT tag in the file.
        OK);                          -- An OK tag in the file.

   type Compilation_Unit_Kinds is (
        Package_Specification,
        Generic_Package,
        Package_Body,                  -- May complete a generic package.
        Procedure_Specification,
        Generic_Procedure,
        Procedure_Body,                -- May complete a generic procedure.
        Function_Specification,
        Generic_Function,
        Function_Body,                 -- May complete a generic function.
        Package_Instantiation,
        Procedure_Instantiation,
        Function_Instantiation,
        Package_Renaming,
        Procedure_Renaming,
        Function_Renaming,
        Generic_Package_Renaming,
        Generic_Procedure_Renaming,
        Generic_Function_Renaming,
        Package_Subunit,
        Procedure_Subunit,
        Function_Subunit,
        Task_Subunit,
        Protected_Subunit,
        Configuration_Pragma);

   subtype PE_Label is String(1..10);
      -- The type of a set label for a possible error.

   subtype Comp_Unit_Name is String(1..250);
      -- The type of a compilation unit name.

   type Info_Record (Kind : Info_Kind_Type := Unknown) is record
      Source_Name : Trace.Name_Subtype;
          -- This is the simple name of the source file.
      Start_Line     : Trace.Line_Number_Type;
      Start_Position : Trace.Line_Position_Type;
      End_Line       : Trace.Line_Number_Type;
      End_Position   : Trace.Line_Position_Type;
         -- For Kind = Compilation_Unit, these values give the start
         -- and end of the compilation unit. These values must include
         -- all of the significant text of the unit, but may or may not
         -- include leading and trailing whitespace and comments.
         -- Generally, this information is used primarily
         -- to determine which errors belong to a particular unit, and
         -- secondarily to determine that all of the units of a particular
         -- file have been processed.
         -- For the four Error Kinds (as well as ANNEX_C and OK), these
         -- represent the error limits
         -- for a passing result for this tag. (We use the same limits for
         -- an OK tag, as if it had been ERROR instead.) The exact limits
         -- depend on whether a range indicator is given or not, and
         -- may be expanded in some cases; but at a minimum they
         -- always include the entire line that contains the tag.
         -- The positions are only used in grading in strict mode; in
         -- normal mode (the one used for formal conformity assessments),
         -- only the reported line numbers are used. (As such, the reported
         -- line/positions should only include other lines if some non-trivial
         -- text is involved - not comments or whitespace).
      case Kind is
         when Unknown => null;
         when Compilation_Unit =>
            Unit_Kind : Compilation_Unit_Kinds;
               -- The kind of compilation unit.
            Unit_Name : Comp_Unit_Name;
               -- The space-padded name of the compilation unit. If the
               -- name is longer than this component, it will be truncated.
               -- (This is mainly for debugging.)
            Is_Main   : Boolean;
               -- Is this unit the main subprogram for this test?
            Optional  : Boolean;
               -- Is processing this unit optional? (This is not determinable
               -- by reading the source, but rather the comments, so we
               -- probably will need a list of such units to feed into
               -- the tool that creates these records.)
         when Possible_Error =>
            Set_Label : PE_Label;
               -- The space-padded set label for this possible error.
         when Error | Optional_Error | NA_Error | Annex_C_Requirement | OK =>
            null;
      end case;
   end record;

   -- Notes:
   --
   -- With the exception of the Optional flag, this data can be obtained
   -- by analysis (and a bit of parsing) of each test file. The Summary
   -- tool will create this data.
   --
   -- We can get various information from the source name, so we don't need
   -- to include that in these records. In particular, all of the records
   -- whose source names match in the first 7 characters make up a single
   -- test. Additionally, the test class is determined by the first character.
   -- (Careful: this is not necessarily true for compilation unit names.)


   -- A test file summary is a .CSV file (see Trace.A for a discussion of
   -- the reasons for chosing .CSV files) containing records of
   -- Info_Record_Type.
   --
   -- In order to simplify the file a bit, we share a few fields.
   --
   -- A test file summary contains the following fields:
   --
   -- Kind: (includes the Kind and Unit_Kind information)
   --    ERROR (Error), PERROR (Possible_Error), OERROR (Optional_Error),
   --    NAERR (NA_Error), ACRQMT (Annex_C_Requirement), OK (OK),
   --    All of the following are Compilation_Unit kinds, with the indicated
   --    Unit_Kind:
   --    UPACKSPEC (Package_Specification), UFUNCSPEC (Function_Specification),
   --    UPROCSPEC (Procedure_Specification), UGENPACK (Generic_Package),
   --    UGENFUNC (Generic_Function), UGENPROC (Generic_Procedure),
   --    UPACKBODY (Package_Body), UFUNCBODY (Function_Body),
   --    UPROCBODY (Procedure_Body),
   --    UPACKINST (Package_Instantiation), UFUNCINST (Function_Instantiation),
   --    UPROCINST (Procedure_Instantiation), UPACKREN (Package_Renaming),
   --    UFUNCREN (Function_Renaming), UPROCREN (Procedure_Renaming),
   --    UGPACKREN (Generic_Package_Renaming),
   --    UGFUNCREN (Generic_Function_Renaming),
   --    UGPROCREN (Generic_Procedure_Renaming),
   --    PACKSUB (Package_Subunit), PROCSUB (Procedure_Subunit),
   --    FUNCSUB (Function_Subunit), TASKSUB (Task_Subunit),
   --    PROTSUB (Protected_Subunit), PRAGMA (Configuration_Pragma)
   -- Source_Name: The quoted name of the source file
   --       (as described in the record definition).
   -- Start_Line, Start_Pos, End_Line, End_Pos: The line and positions as
   --       integer values (the meaning is as described in the record
   --       definition).
   -- Name_Label: The quoted Unit_Name or Set_Label. If neither of these
   --       components is appropriate for the Kind, it can be omitted other
   --       than the comma.
   -- Flag: Only for compilation units; for others it can be omitted
   --       completely (since it is last, no comma is needed, either).
   --       MAIN (Is_Main:=True,Optional:=False); OPTMAIN (Is_Main:=True,
   --       Optional:=True); OPT (Is_Main:=False,Optional:=True); nothing
   --       is (Is_Main:=False,Optional:=False)

   procedure Write_Summary_Record (Summary_File : in Ada.Text_IO.File_Type;
                                   Rec : in Test_Summary.Info_Record);
      -- Write a summary record to Summary_File.

end Test_Summary;

with Ada.Strings.Fixed;
package body Test_Summary is

   DEBUG : constant Boolean := True;

   procedure Write_Summary_Record (Summary_File : in Ada.Text_IO.File_Type;
                                   Rec : in Test_Summary.Info_Record) is
      use Test_Summary;
   begin
      case Rec.Kind is
         when Test_Summary.Unknown =>
            Ada.Text_IO.Put (Summary_File, "UNKN,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Unknown at line");
            end if;
         when Test_Summary.Error =>
            Ada.Text_IO.Put (Summary_File, "ERROR,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Error at line");
            end if;
         when Test_Summary.Optional_Error =>
            Ada.Text_IO.Put (Summary_File, "OERROR,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Optional Error at line");
            end if;
         when Test_Summary.Possible_Error =>
            Ada.Text_IO.Put (Summary_File, "PERROR,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Possible Error at line");
            end if;
         when Test_Summary.NA_Error =>
            Ada.Text_IO.Put (Summary_File, "NAERR,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Not Applicable Error at line");
            end if;
         when Test_Summary.OK =>
            Ada.Text_IO.Put (Summary_File, "OK,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing OK Error at line");
            end if;
         when Test_Summary.Annex_C_Requirement =>
            Ada.Text_IO.Put (Summary_File, "ACRQMT,");
            if DEBUG then
                Ada.Text_IO.Put ("Writing Annex C Rqmnt at line");
            end if;
         when Test_Summary.Compilation_Unit =>
            case Rec.Unit_Kind is
               when Test_Summary.Package_Specification =>
                  Ada.Text_IO.Put (Summary_File, "UPACKSPEC,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pack spec starting at line");
                  end if;
               when Test_Summary.Function_Specification =>
                  Ada.Text_IO.Put (Summary_File, "UFUNCSPEC,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing func spec starting at line");
                  end if;
               when Test_Summary.Procedure_Specification =>
                  Ada.Text_IO.Put (Summary_File, "UPROCSPEC,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing proc spec starting at line");
                  end if;
               when Test_Summary.Generic_Package =>
                  Ada.Text_IO.Put (Summary_File, "UGENPACK,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing generic pack starting at line");
                  end if;
               when Test_Summary.Generic_Function =>
                  Ada.Text_IO.Put (Summary_File, "UGENFUNC,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing generic func starting at line");
                  end if;
               when Test_Summary.Generic_Procedure =>
                  Ada.Text_IO.Put (Summary_File, "UGENPROC,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing generic proc starting at line");
                  end if;
               when Test_Summary.Package_Body =>
                  Ada.Text_IO.Put (Summary_File, "UPACKBODY,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pack body starting at line");
                  end if;
               when Test_Summary.Function_Body =>
                  Ada.Text_IO.Put (Summary_File, "UFUNCBODY,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing func body starting at line");
                  end if;
               when Test_Summary.Procedure_Body =>
                  Ada.Text_IO.Put (Summary_File, "UPROCBODY,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing proc body starting at line");
                  end if;
               when Test_Summary.Package_Instantiation =>
                  Ada.Text_IO.Put (Summary_File, "UPACKINST,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pack inst starting at line");
                  end if;
               when Test_Summary.Function_Instantiation =>
                  Ada.Text_IO.Put (Summary_File, "UFUNCINST,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing func inst starting at line");
                  end if;
               when Test_Summary.Procedure_Instantiation =>
                  Ada.Text_IO.Put (Summary_File, "UPROCINST,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing proc inst starting at line");
                  end if;
               when Test_Summary.Package_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UPACKREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pack rename starting at line");
                  end if;
               when Test_Summary.Function_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UFUNCREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing func rename starting at line");
                  end if;
               when Test_Summary.Procedure_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UPROCREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing proc rename starting at line");
                  end if;
               when Test_Summary.Generic_Package_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UGPACKREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing gen pack ren starting at line");
                  end if;
               when Test_Summary.Generic_Function_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UGFUNCREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing gen func ren starting at line");
                  end if;
               when Test_Summary.Generic_Procedure_Renaming =>
                  Ada.Text_IO.Put (Summary_File, "UGPROCREN,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing gen proc ren starting at line");
                  end if;
               when Test_Summary.Package_Subunit =>
                  Ada.Text_IO.Put (Summary_File, "PACKSUB,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pack subunit starting at line");
                  end if;
               when Test_Summary.Procedure_Subunit =>
                  Ada.Text_IO.Put (Summary_File, "PROCSUB,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing proc subunit starting at line");
                  end if;
               when Test_Summary.Function_Subunit =>
                  Ada.Text_IO.Put (Summary_File, "FUNCSUB,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing func subunit starting at line");
                  end if;
               when Test_Summary.Task_Subunit =>
                  Ada.Text_IO.Put (Summary_File, "TASKSUB,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing task subunit starting at line");
                  end if;
               when Test_Summary.Protected_Subunit =>
                  Ada.Text_IO.Put (Summary_File, "PROTSUB,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing prot subunit starting at line");
                  end if;
               when Test_Summary.Configuration_Pragma =>
                  Ada.Text_IO.Put (Summary_File, "PRAGMA,");
                  if DEBUG then
                     Ada.Text_IO.Put ("Writing pragma starting at line");
                  end if;
            end case;
      end case;

      Ada.Text_IO.Put (Summary_File,
                       Ada.Strings.Fixed.Trim (Rec.Source_Name,
                          Ada.Strings.Both) & ',');

      Ada.Text_IO.Put (Summary_File,
                       Ada.Strings.Fixed.Trim (
                          Trace.Line_Number_Type'Image(Rec.Start_Line),
                          Ada.Strings.Both) & ',');
      if DEBUG then
         Ada.Text_IO.Put (Trace.Line_Number_Type'Image(Rec.Start_Line));
      end if;

      Ada.Text_IO.Put (Summary_File,
                       Ada.Strings.Fixed.Trim (
                          Trace.Line_Position_Type'Image(Rec.Start_Position),
                          Ada.Strings.Both) & ',');

      Ada.Text_IO.Put (Summary_File,
                       Ada.Strings.Fixed.Trim (
                          Trace.Line_Number_Type'Image(Rec.End_Line),
                          Ada.Strings.Both) & ',');

      Ada.Text_IO.Put (Summary_File,
                       Ada.Strings.Fixed.Trim (
                          Trace.Line_Position_Type'Image(Rec.End_Position),
                          Ada.Strings.Both) & ',');

      case Rec.Kind is
         when Possible_Error =>
            Ada.Text_IO.Put (Summary_File,
                             Ada.Strings.Fixed.Trim (Rec.Set_Label,
                                Ada.Strings.Both) & ',');
            if DEBUG then
               Ada.Text_IO.Put ("; with label=" & Ada.Strings.Fixed.Trim (
                                Rec.Set_Label, Ada.Strings.Both));
            end if;
         when Compilation_Unit =>
            Ada.Text_IO.Put (Summary_File,
                             Ada.Strings.Fixed.Trim (Rec.Unit_Name,
                                Ada.Strings.Both) & ',');
            if DEBUG then
               Ada.Text_IO.Put ("; with name=" & Ada.Strings.Fixed.Trim (
                                Rec.Unit_Name, Ada.Strings.Both));
            end if;
            if Rec.Is_Main then
               if Rec.Optional then
                  Ada.Text_IO.Put (Summary_File, "OPTMAIN");
               else
                  Ada.Text_IO.Put (Summary_File, "MAIN");
               end if;
            else
               if Rec.Optional then
                  Ada.Text_IO.Put (Summary_File, "OPT");
               else
                  null; -- No flags.
               end if;
            end if;

         when Error | Optional_Error | NA_Error | Annex_C_Requirement |
              OK | Unknown =>
            Ada.Text_IO.Put (Summary_File,",");
      end case;

      Ada.Text_IO.New_Line (Summary_File);
      if DEBUG then
         Ada.Text_IO.New_Line;
      end if;
   end Write_Summary_Record;

end Test_Summary;
