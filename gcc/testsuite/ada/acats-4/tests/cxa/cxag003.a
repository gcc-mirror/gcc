-- CXAG003.A
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
-- OBJECTIVE:
--     Check that function Ada.Directories.Name_Case_Equivalence exists and
--     that the value returned reflects the actual handling of file names.
--
-- TEST DESCRIPTION:
--     The usual usage of Name_Case_Equivalence is to determine the handling
--     of file names provided externally (for instance, on the command line,
--     or via an imput box). The value of Name_Case_Equivalence could then
--     be used to determine whether or not to normalize the file name before
--     doing further operations.
--
--     We treat the file name as being externally provided by converting it to
--     various cases. We then try to read the file with those file names,
--     and checking the behavior against the reported value of
--     Name_Case_Equivalence for the original file.

-- APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support creating,
--     opening, and deleting a text file, and that support multiple file
--     names in both upper and lower case.
--
-- CHANGE HISTORY:
--     04 Feb 2018  SJW  Drafted test.
--     05 Feb 2018  RLB  Strengthened test.
--     04 May 2018  RLB  Corrected Read_File to return immediately when the
--                       file cannot be opened.
--
--!
with Report;
with Ada.Directories;
with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Characters.Handling;
procedure CXAG003 is

   TC_Trace : constant Boolean := False; -- Output extra trace information.

   Not_Applicable_Error : exception;

   procedure Create_File (Name : in String) is
      -- Create and write the file given by Name.
      -- Raised Not_Applicable_Error if this operation is not supported.
      Test_File : Ada.Text_IO.File_Type;
   begin
      Test_for_Text_IO_Support:
      begin

         -- An implementation that does not support Text_IO in a particular
         -- environment will raise Use_Error on calls to various
         -- Text_IO operations.  This block statement encloses a call to
         -- Create, which should raise the exception in a non-supportive
         -- environment.  This exception will be handled to produce a
         -- Not_Applicable result.

         Ada.Text_IO.Create (File => Test_File,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Name);

         if TC_Trace then
            Report.Comment ("Original file name = " & Name);
         end if;
      exception

         when Ada.Text_IO.Use_Error | Ada.Text_IO.Name_Error =>
            Report.Not_Applicable ("Text files not supported - " &
                                   "Create as Out_File for Ada.Text_IO");
            raise Not_Applicable_Error;

      end Test_for_Text_IO_Support;

      Ada.Text_IO.Put_Line (Test_File, "Some data here");
      Ada.Text_IO.Close (Test_File);

   exception
      when Not_Applicable_Error => raise;
      when Err : others =>
         Report.Failed ("Unexpected exception writing Test_File");
         if TC_Trace then
            Report.Comment ("Info: " &
                            Ada.Exceptions.Exception_Information (Err));
         end if;
   end Create_File;


   procedure Read_File (Name : in String;
                        Kind : in Ada.Directories.Name_Case_Kind) is
      -- Read the file given by Name.
      -- Raised Not_Applicable_Error if this operation is not supported.
      Test_File : Ada.Text_IO.File_Type;
   begin
      begin
         Ada.Text_IO.Open (File => Test_File,
                           Mode => Ada.Text_IO.In_File,
                           Name => Name);

         -- If we get here, the file was successfully opened.
         case Kind is
            when Ada.Directories.Case_Sensitive =>
               Report.Failed ("Case_Sensitive file system opened file with " &
                              "name in different case");
            when Ada.Directories.Case_Insensitive |
                 Ada.Directories.Case_Preserving =>
               -- Can't easily tell between these.
               if TC_Trace then
                  Report.Comment ("Case insensitive file opened correctly");
               end if;
            when Ada.Directories.Unknown =>
               Report.Comment ("Unknown file system acts case insensitive");
         end case;

         if TC_Trace then
            Report.Comment ("Check file name = " & Name);
         end if;

      exception
         when Ada.Text_IO.Name_Error =>
            -- The file is not found (Name should be the result of
            -- of a successful call to Ada.Directories.Full_Name).
            -- This should only happen on a Case_Sensitive system.
            case Kind is
               when Ada.Directories.Case_Sensitive =>
                  if TC_Trace then
                     Report.Comment ("Unable to open case sensitive file " &
                                     "as expected");
                  end if;
               when Ada.Directories.Case_Insensitive |
                    Ada.Directories.Case_Preserving =>
                  -- Can't easily tell between these.
                  Report.Failed ("Case_Insensitive/Case_Preserving file " &
                                 "system unable to open file with " &
                                 "name in different case");
               when Ada.Directories.Unknown =>
                  Report.Comment ("Unknown file system acts case sensitive");
            end case;

            if TC_Trace then
               Report.Comment ("Check file name = " & Name);
            end if;
            return; -- File is not open, so we're done here (nothing to read).
      end;

      declare
         Buffer : String(1..80);
         Last   : Natural := 0;
      begin
         Ada.Text_IO.Get_Line (Test_File, Buffer, Last);
         if Buffer(1..Last) /= "Some data here" then
            Report.Failed ("Wrong data read from file");
         end if;
      end;

      Ada.Text_IO.Close (Test_File);

   exception
      when Err : others =>
         Report.Failed ("Unexpected exception reading Test_File");
         if TC_Trace then
            Report.Comment ("Info: " &
                            Ada.Exceptions.Exception_Information (Err));
         end if;
   end Read_File;

begin
   Report.Test ("CXAG003",
                "Check that function Ada.Directories.Name_Case_Equivalence " &
                 "exists and that the value returned reflects the actual " &
                 "handling of file names");

   declare
       Original_File_Simple_Name : constant String :=
          Report.Legal_File_Name (X => 1); -- Call only after
                                           -- calling Report.Test.
       Upper_File_Simple_Name : constant String :=
          Ada.Characters.Handling.To_Upper (Original_File_Simple_Name);
       Lower_File_Simple_Name : constant String :=
          Ada.Characters.Handling.To_Lower (Original_File_Simple_Name);
   begin
       if Original_File_Simple_Name = Upper_File_Simple_Name and then
          Original_File_Simple_Name = Lower_File_Simple_Name then
          -- There are no characters in the file name which can change case.
          -- This won't happen for the default version of Report, but it might
          -- if Report is customized.
          Report.Not_Applicable ("File names are not case sensitive");
          raise Not_Applicable_Error;
       end if;

       begin
          declare
             Original_File_Full_Name : constant String :=
                 Ada.Directories.Full_Name (Original_File_Simple_Name);
             Kind : Ada.Directories.Name_Case_Kind;
          begin
             -- Check that both alternate file names are acceptable to the
             -- implementation (Name_Error should be raised, if, for instance,
             -- file names in lower case are not allowed).
             begin
                declare
                   Upper_File_Full_Name : constant String :=
                      Ada.Directories.Full_Name (Upper_File_Simple_Name);
                   Lower_File_Full_Name : constant String :=
                      Ada.Directories.Full_Name (Lower_File_Simple_Name);
                begin
                   if Upper_File_Full_Name = Lower_File_Full_Name then
                      -- Use the file names to prevent a compiler from removing
                      -- them as dead. These names should be different, as the
                      -- original names are different.
                      Report.Comment ("Don't optimize file names");
                   end if;
                end;
             exception
                when Ada.Directories.Name_Error =>
                   Report.Not_Applicable ("Case changed file name is not "
                                        & "allowed by implementation");
                   raise Not_Applicable_Error;
             end;
             Create_File (Original_File_Full_Name);
             -- If we get here, the test is applicable and the Test File has
             -- been successfully created.

             begin
                Kind := Ada.Directories.Name_Case_Equivalence (
                           Original_File_Full_Name);
                   -- We're passing the result of a call to Full_Name here, so
                   -- if this raises an exception, either Full_Name or
                   -- Name_Case_Equivalence is buggy.
                Report.Comment ("Name_Case_Equivalence returned "
                            & Ada.Directories.Name_Case_Kind'Image (Kind));
             exception
                when Ada.Directories.Name_Error =>
                    Report.Failed ("Name_Case_Equivalence does not accept " &
                                   "result of Full_Name");
                when Err : others =>
                    Report.Failed ("Unexpected exception from " &
                                   "Name_Case_Equivalence");
                    if TC_Trace then
                        Report.Comment ("Info: " &
                                   Ada.Exceptions.Exception_Information (Err));
                    end if;
             end;

             -- One of the following has to be true, as we previously tested
             -- that both are not false. This is the meat of the test.
             if Original_File_Simple_Name /= Upper_File_Simple_Name then
                Read_File (Ada.Directories.Full_Name (Upper_File_Simple_Name),
                           Kind => Kind);
             end if;
             if Original_File_Simple_Name /= Lower_File_Simple_Name then
                Read_File (Ada.Directories.Full_Name (Lower_File_Simple_Name),
                           Kind => Kind);
             end if;

             -- Clean up the test file:
             Ada.Directories.Delete_File (Original_File_Full_Name);

          exception
             when Err : Ada.Directories.Name_Error =>
                Report.Failed ("Unexpected Name_Error");
                if TC_Trace then
                   Report.Comment ("Info: " &
                                   Ada.Exceptions.Exception_Information(Err));
                end if;
          end;
       exception
          when Ada.Directories.Name_Error =>
             Report.Not_Applicable ("Cannot get full name of test file name");
             raise Not_Applicable_Error;
       end;

   end;
   Report.Result;
exception
   when Not_Applicable_Error =>
      -- The test is Not Applicable for a reason already reported.
      Report.Result;
end CXAG003;
