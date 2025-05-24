-- CXAG001.A
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
--     Check that package Ada.Directories exists and can be used to
--     create and delete directories, change the default directory, and
--     copy files.
--
-- TEST DESCRIPTION:
--     We try using the operations in Ada.Directories as a programmer might,
--     emulating saving a file in a temporary directory, then copying the
--     final result to the original location.
--
--     Specifically, we create a temporary directory in the current default
--     directory, and create a text file there by constructing the name using
--     various Ada.Directories functions. We then change the default directory
--     to the temporary directory, create another temporary file there,
--     and attempt to read the first file based using only its simple name.
--     We then return the default directory to its original value, attempt
--     to read the second file using its full name, then copy the first
--     temporary file there. We then delete the temporary directory,
--     check that the copy of the first file is still accessible and the
--     original is not. Finally, we delete the file copy.
--
--     The Ada.Directory operations tested (in order of appearance in the
--     standard package) are:
--         Current_Directory
--         Set_Directory
--         Create_Directory
--         Delete_Directory
--         Delete_Tree
--         Delete_File
--         Copy_File
--         Full_Name
--         Simple_Name
--         Containing_Directory
--         Compose
--         Exists
--
--     If the target execution environment does not support creating
--     directories, the Impdef constant Directory_To_Create
--     MUST be set to the null string. If it is not, the test will fail with
--     an unexpected exception.
--
--     We assume that the Form of "" allows the contents of a newly created
--     directory to be written by the program that created it. This is not
--     required by the Ada Standard (it has nothing to say about the meaning
--     of Form = "" other than it specifies the default options), but it
--     would be so unfriendly to programmers ("The default is that you
--     can't use the directory that you just created? Really?") that we
--     simply assume it is the case.
--
-- APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support all of the
--     following:
--        * Creating and deleting directories;
--        * Reading and setting the current default directory; and
--        * Creating, opening, and deleting a text file.
--
-- CHANGE HISTORY:
--      9 Feb 2015  RLB  Created test.
--     19 Feb 2015  RLB  Changed to use Impdef.Equivalent_File_Names. Added
--                       uses of Exists.
--     12 Mar 2015  RLB  Fixed overlength lines.
--
--!
with Report;
with Ada.Directories;
with Impdef;
with Ada.Exceptions;
with Ada.Text_IO;
procedure CXAG001 is

   TC_Trace : constant Boolean := False; -- Output extra trace information.

   type Str_Ptr is access String;

   Temp_Directory_Simple_Name : constant Str_Ptr :=
                       new String'(Impdef.Directory_To_Create);
   Temp_Directory_Full_Name : Str_Ptr;
   Initial_Default_Directory : Str_Ptr;
   Draft_File1_Simple_Name : Str_Ptr;
   Draft_File1_Full_Name : Str_Ptr;
   Draft_File2_Simple_Name : Str_Ptr;
   Draft_File2_Full_Name : Str_Ptr;

   Draft_File1 : Ada.Text_IO.File_Type;
   Draft_File2 : Ada.Text_IO.File_Type;

begin
   Report.Test ("CXAG001", "Check that package Ada.Directories exists " &
                           "and can be used to " &
                           "create and delete directories, change the " &
                           "default directory, and copy files");

   if Impdef.Directory_To_Create = "" then
      Report.Not_Applicable ("No directory to create defined");
      goto Complete;
   end if;

   begin
      Initial_Default_Directory :=
         new String'(Ada.Directories.Current_Directory);
      if TC_Trace then
         Report.Comment ("Initial default directory=" &
            Initial_Default_Directory.all);
      end if;
   exception
      when Ada.Directories.Use_Error =>
         Report.Not_Applicable ("Unable to retrieve " &
                                "current default directory");
         goto Complete;
   end;


   begin
      Ada.Directories.Create_Directory (Temp_Directory_Simple_Name.all);
      if TC_Trace then
         Report.Comment ("Create directory=" & Temp_Directory_Simple_Name.all);
      end if;
   exception
      when Ada.Directories.Name_Error =>
         Report.Failed ("Unable to create temporary directory - Bad name=" &
            Temp_Directory_Simple_Name.all);
         goto Complete;
         -- This is considered a failure as the provided name is expected to
         -- be syntactically correct for the name of a directory. If no such
         -- name exists, then the name should have been set to "".
      when Ada.Directories.Use_Error =>
         Report.Failed ("Unable to create temporary directory - permissions");
         goto Complete;
         -- This is considered a failure as the provided name is expected to
         -- be an appropriate name for a temporary directory; if it might not
         -- work for any reason, some other name should be provided.
   end;

   -- Get the simple name of the temporary files from Report:
   Draft_File1_Simple_Name := new String'(Report.Legal_File_Name (X => 1));
   Draft_File2_Simple_Name := new String'(Report.Legal_File_Name (X => 2));

   -- We want to get the full name of the directory so we can create a file
   -- using the full name. There are two ways to do that, and we'll use
   -- both to ensure that they give the same results.

   Temp_Directory_Full_Name :=
      new String'(Ada.Directories.Full_Name (Temp_Directory_Simple_Name.all));
   if TC_Trace then
      Report.Comment ("Directory full name=" & Temp_Directory_Full_Name.all);
   end if;

   if not Impdef.Equivalent_File_Names (Temp_Directory_Full_Name.all,
      Ada.Directories.Compose (Containing_Directory =>
                                        Initial_Default_Directory.all,
                               Name => Temp_Directory_Simple_Name.all)) then
      Report.Failed ("Different results for Full_Name and Compose");
      if TC_Trace then
         Report.Comment ("Directory full name via Compose=" &
           Ada.Directories.Compose (
              Containing_Directory => Initial_Default_Directory.all,
              Name => Temp_Directory_Simple_Name.all));
      end if;
   end if;

   -- Check that we can create a Text_IO file anywhere.

   begin
      Test_for_Text_IO_Support:
      begin

         -- An implementation that does not support Text_IO in a particular
         -- environment will raise Use_Error on calls to various
         -- Text_IO operations.  This block statement encloses a call to
         -- Create, which should raise the exception in a non-supportive
         -- environment.  This exception will be handled to produce a
         -- Not_Applicable result.

         Ada.Text_IO.Create (File => Draft_File1,
                             Mode => Ada.Text_IO.Out_File,
                             Name => Draft_File1_Simple_Name.all);

      exception

         when Ada.Text_IO.Use_Error | Ada.Text_IO.Name_Error =>
            Report.Not_Applicable ("Text files not supported - " &
                                   "Create as Out_File for Ada.Text_IO");
            goto Complete;

      end Test_for_Text_IO_Support;

      Ada.Text_IO.Delete (Draft_File1); -- Don't want this file.

   end;

   -- Attempt to create a file in the temporary directory using its full
   -- name:

   begin
      Ada.Text_IO.Create (File => Draft_File1,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Ada.Directories.Compose (
                                     Containing_Directory =>
                                        Temp_Directory_Full_Name.all,
                                     Name => Draft_File1_Simple_Name.all));
   exception
      when Err : Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
         Report.Failed ("Unable to create temporary file in temporary " &
                        "directory - full file name, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
               Ada.Exceptions.Exception_Message (Err));
            Report.Comment ("Creating " &
                  Ada.Directories.Compose (
                     Containing_Directory =>
                        Temp_Directory_Full_Name.all,
                     Name => Draft_File1_Simple_Name.all));
         end if;
   end;

   -- Change the default directory to the temporary directory:

   begin
      Ada.Directories.Set_Directory (Temp_Directory_Full_Name.all);
      if TC_Trace then
         Report.Comment ("Set default directory=" &
                         Temp_Directory_Full_Name.all);
      end if;
   exception
      when Ada.Directories.Name_Error =>
         Report.Failed ("Unable to set default directory - Bad name=" &
            Temp_Directory_Full_Name.all);
         goto Complete;
         -- This is considered a failure as the name was returned from
         -- Full_Name so it should continue to be OK. (The syntax of a file
         -- name can't change just because we opened a file.)
      when Ada.Directories.Use_Error =>
         Report.Not_Applicable ("Changing the default directory " &
                                "not supported");
         if TC_Trace then
            Report.Comment ("Can't set the default directory to " &
               Temp_Directory_Full_Name.all);
         end if;
         goto Complete;
   end;

   -- If we get here, we've checked all of the applicability criteria. So
   -- anything that doesn't work from here on is a test failure.

   -- Attempt to create a file in the temporary directory using its simple
   -- name:

   begin
      Ada.Text_IO.Create (File => Draft_File2,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Draft_File2_Simple_Name.all);
   exception
      when Err : Ada.Text_IO.Name_Error | Ada.Text_IO.Use_Error =>
         Report.Failed ("Unable to create temporary file in temporary " &
                        "directory - simple file name, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
            Report.Comment ("Creating " & Draft_File2_Simple_Name.all);
         end if;
   end;

   -- Write some contents into the files and close them, checking the file
   -- names:

   begin
      Ada.Text_IO.Put_Line (Draft_File1, "Full name OK");
      Ada.Text_IO.Put_Line (Draft_File2, "Simple name OK");

      -- Note: In Ada 2005 and later, we have to use Full_Name on both sides,
      -- as Ada.Text_IO.Name might return a full name, or some fragment.
      -- In Ada 83 and Ada 95, Ada.Text_IO.Name had to return a full name.
      Draft_File1_Full_Name := new String'(
         Ada.Directories.Full_Name (Ada.Text_IO.Name (Draft_File1)));
      if not Impdef.Equivalent_File_Names (Draft_File1_Full_Name.all,
         Ada.Directories.Full_Name (Draft_File1_Simple_Name.all)) then
         Report.Failed ("Incorrect full name of file1: " &
                        Ada.Text_IO.Name (Draft_File1));
         if TC_Trace then
            Report.Comment ("Name was " & Draft_File1_Full_Name.all);
            Report.Comment ("Expected " & Ada.Directories.Full_Name (
                                         Draft_File1_Simple_Name.all));
         end if;
      end if;
      Draft_File2_Full_Name := new String'(
         Ada.Directories.Full_Name (Ada.Text_IO.Name (Draft_File2)));
      if not Impdef.Equivalent_File_Names (Draft_File2_Full_Name.all,
         Ada.Directories.Full_Name (Draft_File2_Simple_Name.all)) then
         Report.Failed ("Incorrect full name of file2: " &
                        Ada.Text_IO.Name (Draft_File2));
         if TC_Trace then
            Report.Comment ("Name was " & Draft_File2_Full_Name.all);
            Report.Comment ("Expected " &
                            Ada.Directories.Full_Name (
                                      Draft_File2_Simple_Name.all));
         end if;
      end if;
      -- Check that the containing directory of the files is correct:
      if not Impdef.Equivalent_File_Names (
             Ada.Directories.Containing_Directory (Draft_File1_Full_Name.all),
             Temp_Directory_Full_Name.all) then
         Report.Failed ("Incorrect containing directory of file1: " &
                        Draft_File1_Full_Name.all);
         if TC_Trace then
            Report.Comment ("Name was " &
                            Ada.Directories.Containing_Directory (
                               Draft_File1_Full_Name.all));
            Report.Comment ("Expected " & Temp_Directory_Full_Name.all);
         end if;
      end if;
      if not Impdef.Equivalent_File_Names (
             Ada.Directories.Containing_Directory (Draft_File2_Full_Name.all),
             Temp_Directory_Full_Name.all) then
         Report.Failed ("Incorrect containing directory of file2: " &
                        Draft_File2_Full_Name.all);
         if TC_Trace then
            Report.Comment ("Name was " &
                            Ada.Directories.Containing_Directory (
                                              Draft_File2_Full_Name.all));
            Report.Comment ("Expected " & Temp_Directory_Full_Name.all);
         end if;
      end if;
      -- Check that the simple name of the files is correct:
      if not Impdef.Equivalent_File_Names (
            Ada.Directories.Simple_Name (Draft_File1_Full_Name.all),
            Draft_File1_Simple_Name.all) then
         Report.Failed ("Incorrect simple name of file1: " &
                        Draft_File1_Full_Name.all);
         if TC_Trace then
            Report.Comment ("Name was " & Ada.Directories.Simple_Name (
                                              Draft_File1_Full_Name.all));
            Report.Comment ("Expected " & Draft_File1_Simple_Name.all);
         end if;
      end if;
      if not Impdef.Equivalent_File_Names (
          Ada.Directories.Simple_Name (Draft_File2_Full_Name.all),
          Draft_File2_Simple_Name.all) then
         Report.Failed ("Incorrect simple name of file2: " &
                        Draft_File2_Full_Name.all);
         if TC_Trace then
            Report.Comment ("Name was " & Ada.Directories.Simple_Name (
                                              Draft_File2_Full_Name.all));
            Report.Comment ("Expected " & Draft_File2_Simple_Name.all);
         end if;
      end if;

      Ada.Text_IO.Close (Draft_File1);
      Ada.Text_IO.Close (Draft_File2);
      if TC_Trace then
         Report.Comment ("Write both files");
      end if;

   exception
      when Err : others =>
         Report.Failed ("Unexpected exception writing text files, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- Check that the first file exists using its full name:
   if not Ada.Directories.Exists (Ada.Directories.Compose (
                     Containing_Directory => Temp_Directory_Full_Name.all,
                     Name => Draft_File1_Simple_Name.all)) then
      Report.Failed ("The file we just created does not exist " &
                     " (Draft1 full name)");
   end if;

   -- Check that the first file exists using its simple name:
   if not Ada.Directories.Exists (Draft_File1_Simple_Name.all) then
      Report.Failed ("The file we just created does not exist " &
                     "(Draft1 simple name)");
   end if;

   -- Check that the second file exists using its full name:
   if not Ada.Directories.Exists (Ada.Directories.Compose (
                     Containing_Directory => Temp_Directory_Full_Name.all,
                     Name => Draft_File2_Simple_Name.all)) then
      Report.Failed ("The file we just created does not exist " &
                     "(Draft2 full name)");
   end if;

   -- Check that the second file exists using its simple name:
   if not Ada.Directories.Exists (Draft_File2_Simple_Name.all) then
      Report.Failed ("The file we just created does not exist " &
                     "(Draft2 simple name)");
   end if;

   -- Attempt to read from the first file, using only its simple name:
   declare
      Var : String(1..4);
   begin
      Ada.Text_IO.Open (Draft_File1, Ada.Text_IO.In_File,
                        Name => Draft_File1_Simple_Name.all);

      Ada.Text_IO.Get (Draft_File1, Var);

      if Var /= "Full" then
         Report.Failed ("Incorrect contents in file");
      end if;
      if TC_Trace then
         Report.Comment ("Read File 1: " & Var);
      end if;

      Ada.Text_IO.Close (Draft_File1);

   exception
      when Err : others =>
         Report.Failed ("Unexpected exception reading text file 1, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- Return the default directory to its original value:

   begin
      Ada.Directories.Set_Directory (Initial_Default_Directory.all);
      if TC_Trace then
         Report.Comment ("(Re)set default directory=" &
                         Initial_Default_Directory.all);
      end if;
   exception
      when Ada.Directories.Name_Error =>
         Report.Failed ("Unable to set default directory - Bad name=" &
            Initial_Default_Directory.all);
      when Ada.Directories.Use_Error =>
         Report.Failed ("Unable to set default directory - permission=" &
            Initial_Default_Directory.all);
            -- This was the original default directory, so of course
            -- we ought to be able to go back to it.
   end;

   -- Attempt to read from the second file, using its full name:
   declare
      Var : String(1..6);
   begin
      Ada.Text_IO.Open (Draft_File2, Ada.Text_IO.In_File,
                        Name => Ada.Directories.Compose (
                                   Containing_Directory =>
                                      Temp_Directory_Full_Name.all,
                                   Name => Draft_File2_Simple_Name.all));

      Ada.Text_IO.Get (Draft_File2, Var);

      if Var /= "Simple" then
         Report.Failed ("Incorrect contents in file");
      end if;
      if TC_Trace then
         Report.Comment ("Read File 2: " & Var);
      end if;

      Ada.Text_IO.Close (Draft_File2);

   exception
      when Err : others =>
         Report.Failed ("Unexpected exception reading text file 2, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- Copy the first file into the current default directory:
   begin
      Ada.Directories.Copy_File (
          Source_Name => Ada.Directories.Compose (
                            Containing_Directory =>
                               Temp_Directory_Full_Name.all,
                            Name => Draft_File1_Simple_Name.all),
          Target_Name => Draft_File1_Simple_Name.all);
      if TC_Trace then
         Report.Comment ("Copy File 1");
      end if;
   exception
      when Err : others =>
         Report.Failed ("Unexpected exception reading text file 2, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- Delete the temporary directory. First we try two common incorrect ways
   -- to delete it in this context:
   begin
      Ada.Directories.Delete_File (Temp_Directory_Simple_Name.all);
      Report.Failed ("Able to delete a directory with Delete_File");
   exception
      when Ada.Directories.Name_Error =>
         if TC_Trace then
            Report.Comment ("Name_Error as expected from deleting " &
                            "a directory with Delete_File");
         end if;
      when Ada.Directories.Use_Error =>
         Report.Failed ("Wrong exception raised by deleting a directory " &
                        "with Delete_File");
   end;

   begin
      Ada.Directories.Delete_Directory (Temp_Directory_Simple_Name.all);
      Report.Failed ("Able to delete a non-empty directory with " &
                     "Delete_Directory");
   exception
      when Ada.Directories.Name_Error =>
         Report.Failed ("Wrong exception raised by deleting a non-empty " &
                        "directory with Delete_Directory");
      when Ada.Directories.Use_Error =>
         if TC_Trace then
            Report.Comment ("Use_Error as expected from deleting a " &
                            "non-empty directory with Delete_File");
         end if;
   end;

   -- OK, now the correct way to delete a non-empty directory:
   -- (We could also have used Search and Delete_File on the contents, but
   -- this test is long enough as it is.)
   begin
      Ada.Directories.Delete_Tree (Temp_Directory_Simple_Name.all);
      if TC_Trace then
         Report.Comment ("Deleted temporary directory with Delete_Tree");
      end if;
   exception
      when Err : others =>
         Report.Failed ("Unexpected exception deleting temporary directory, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- Check that the original first file is gone:
   declare
      Var : String(1..4);
   begin
      Ada.Text_IO.Open (Draft_File1, Ada.Text_IO.In_File,
                        Name => Ada.Directories.Compose (
                               Containing_Directory =>
                                  Temp_Directory_Full_Name.all,
                               Name => Draft_File1_Simple_Name.all));

      Report.Failed ("Able to open draft file that was deleted " &
                     "by Delete_Tree");

      Ada.Text_IO.Delete (Draft_File1); -- Clean-up after this test.

   exception
      when Ada.Text_IO.Name_Error =>
         if TC_Trace then
            Report.Comment ("Name_Error raised as expected from opening " &
                            "non-existent file");
         end if;

      when Err : others =>
         Report.Failed ("Unexpected exception opening non-existent file, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

   -- And finally check that the copy of the first file is still readable:
   declare
      Var : String(1..4);
   begin
      Ada.Text_IO.Open (Draft_File1, Ada.Text_IO.In_File,
                        Name => Draft_File1_Simple_Name.all);

      Ada.Text_IO.Get (Draft_File1, Var);

      if Var /= "Full" then
         Report.Failed ("Incorrect contents in file");
      end if;
      if TC_Trace then
         Report.Comment ("Read Copy of File 1: " & Var);
      end if;

      Ada.Text_IO.Delete (Draft_File1); -- Clean-up after this test.

   exception
      when Err : others =>
         Report.Failed ("Unexpected exception reading copy of text file 1, " &
                        Ada.Exceptions.Exception_Name (Err));
         if TC_Trace then
            Report.Comment ("ExcMess: " &
                            Ada.Exceptions.Exception_Message (Err));
         end if;
   end;

<<Complete>>
   Report.Result;
end CXAG001;
