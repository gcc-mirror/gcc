-- CXAC003.A
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--      Check that the correct exceptions are raised when improperly
--      manipulating stream file objects.
--
-- TEST DESCRIPTION:
--      This test is designed to focus on Stream_IO file manipulation
--      exceptions.  Several potentially common user errors are examined in
--      the test:
--
--      A Status_Error should be raised whenever an attempt is made to perform
--      an operation on a file that is closed.
--
--      A Status_Error should be raised when an attempt is made to open a
--      stream file that is currently open.
--
--      A Mode_Error should be raised when attempting to read from (use the
--      'Read attribute) on an Out_File or Append_Mode file.
--
--      A Mode_Error should be raised when checking for End Of File on a
--      file with mode Out_File or Append_Mode.
--
--      A Mode_Error should be raised when attempting to write to (use the
--      'Output attribute) on a file with mode In_File.
--
--      A Name_Error should be raised when the string provided to the Name
--      parameter of an Open operation does not allow association of an
--      external file.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations capable of supporting
--      external Stream_IO files.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--      02 Mar 01   PHL     Check that Ada.Streams.Stream_IO.Stream raises
--                          Status_Error if the file is not open.  (DR 8652/
--                          0056).
--      15 Mar 01   RLB     Readied for release.
--!

with Ada.Streams.Stream_IO;
with Report;

procedure CXAC003 is

   Stream_File_Object  : Ada.Streams.Stream_IO.File_Type;
   Stream_Access_Value : Ada.Streams.Stream_IO.Stream_Access;
   Stream_Filename     : constant String :=
                              Report.Legal_File_Name ( Nam => "CXAC003" );
   Incomplete : exception;

begin

   Report.Test ("CXAC003", "Check that the correct exceptions are "  &
                           "raised when improperly manipulating stream " &
                           "file objects");

   Test_for_Stream_IO_Support:
   begin
      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on
      -- calls to various Stream_IO operations.  This block statement
      -- encloses a call to Create, which should produce an exception in a
      -- non-supportive environment.  These exceptions will be handled to
      -- produce a Not_Applicable result.

      Ada.Streams.Stream_IO.Create (Stream_File_Object,
                                    Ada.Streams.Stream_IO.Out_File,
                                    Stream_Filename);

   exception

       when Ada.Streams.Stream_IO.Use_Error | Ada.Streams.Stream_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Out_File for Stream_IO" );
          raise Incomplete;

   end Test_for_Stream_IO_Support;

   Operational_Test_Block:
   begin
      -- A potentially common error in a file processing environment
      -- is to attempt to perform an operation on a stream file that is
      -- not currently open. Status_Error should be raised in this case.
      Check_Status_Error:
      begin
         Ada.Streams.Stream_IO.Close (Stream_File_Object);
         -- Attempt to reset a file that is closed.
         Ada.Streams.Stream_IO.Reset (Stream_File_Object,
                                      Ada.Streams.Stream_IO.Out_File);
         Report.Failed ("Exception not raised on Reset of closed file");
      exception
         when Ada.Streams.Stream_IO.Status_Error =>
            null;
         when others       =>
            Report.Failed ("Incorrect exception raised - 1");
      end Check_Status_Error;

      -- A similar error is to use Ada.Streams.Stream_IO.Stream
      -- to attempt to perform an operation on a stream file that is
      -- not currently open. Status_Error should be raised in this case.
      -- (Defect Report 8652/0046, as reflected in Technical Corrigendum 1.)
      Check_Status_Error2:
      begin
         -- Ensure that the file is not open.
         if Ada.Streams.Stream_Io.Is_Open (Stream_File_Object) then
            Ada.Streams.Stream_Io.Close (Stream_File_Object);
         end if;
         Stream_Access_Value :=
            Ada.Streams.Stream_Io.Stream (Stream_File_Object);
         Report.Failed ("Exception not raised on Stream of closed file");
      exception
         when Ada.Streams.Stream_Io.Status_Error =>
            null;
         when others =>
            Report.Failed ("Incorrect exception raised - 2");
      end Check_Status_Error2;

      -- Another potentially common error in a file processing environment
      -- is to attempt to Open a stream file that is currently open.
      -- Status_Error should be raised in this case.
      Check_Status_Error3:
      begin
         -- Ensure that the file is open.
         if not Ada.Streams.Stream_IO.Is_Open (Stream_File_Object) then
            Ada.Streams.Stream_IO.Open (Stream_File_Object,
                                        Ada.Streams.Stream_IO.In_File,
                                        Stream_Filename);
         end if;
         Ada.Streams.Stream_IO.Open (Stream_File_Object,
                                     Ada.Streams.Stream_IO.In_File,
                                     Stream_Filename);
         Report.Failed ("Exception not raised on Open of open file");
      exception
         when Ada.Streams.Stream_IO.Status_Error =>
            null;
         when others       =>
            Report.Failed ("Incorrect exception raised - 3");
      end Check_Status_Error3;

      -- Another example of a potential error occurring in a file
      -- processing environment is to attempt to use the 'Read attribute
      -- on a stream file that is currently in Out_File or Append_File
      -- mode. Mode_Error should be raised in both of these cases.
      Check_Mode_Error:
      declare
         Int_Var : Integer := -10;
      begin

         Reset1:
         begin
            Ada.Streams.Stream_IO.Reset (Stream_File_Object,
                                         Ada.Streams.Stream_IO.Out_File);
         exception
            when Ada.Streams.Stream_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to Out_File not supported for Stream_IO - 1" );
               raise Incomplete;
         end Reset1;

         Stream_Access_Value :=
              Ada.Streams.Stream_IO.Stream (Stream_File_Object);
         Integer'Write (Stream_Access_Value, Int_Var);

         -- File contains an integer value, but is of mode Out_File.
         Integer'Read (Stream_Access_Value, Int_Var);
            Report.Failed ("Exception not raised by 'Read of Out_File");
      exception
         when Incomplete =>
            raise;
         when Ada.Streams.Stream_IO.Mode_Error =>
            null;
            Try_Read:
            begin
               Reset2:
               begin
                  Ada.Streams.Stream_IO.Reset
                    (Stream_File_Object, Ada.Streams.Stream_IO.Append_File);
               exception
                  when Ada.Streams.Stream_IO.Use_Error =>
                     Report.Not_Applicable
                        ( "Reset to Append_File not supported " &
                          "for Stream_IO - 2" );
                     raise Incomplete;
               end Reset2;

               Integer'Write (Stream_Access_Value, Int_Var);
               -- Attempt read from Append_File mode file.
               Integer'Read (Stream_Access_Value, Int_Var);
               Report.Failed
                  ("Exception not raised by 'Read of Append file");
            exception
               when Incomplete =>
                  null;
               when Ada.Streams.Stream_IO.Mode_Error =>
                  null;
               when others     =>
                  Report.Failed ("Incorrect exception raised - 4b");
            end Try_Read;

         when others => Report.Failed ("Incorrect exception raised - 4a");
      end Check_Mode_Error;

      -- Another example of a this type of potential error is to attempt
      -- to check for End Of File on a stream file that is currently in
      -- Out_File or Append_File mode. Mode_Error should also be raised
      -- in both of these cases.
      Check_End_File:
      declare
         Test_Boolean : Boolean := False;
      begin
         Reset3:
         begin
            Ada.Streams.Stream_IO.Reset (Stream_File_Object,
                                         Ada.Streams.Stream_IO.Out_File);
         exception
            when Ada.Streams.Stream_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to Out_File not supported for Stream_IO - 3" );
               raise Incomplete;
         end Reset3;

         Test_Boolean :=
           Ada.Streams.Stream_IO.End_Of_File (Stream_File_Object);
         Report.Failed ("Exception not raised by EOF on Out_File");
      exception
         when Incomplete =>
            null;
         when Ada.Streams.Stream_IO.Mode_Error =>
            null;
            EOF_For_Append_File:
            begin
               Reset4:
               begin
                  Ada.Streams.Stream_IO.Reset
                    (Stream_File_Object, Ada.Streams.Stream_IO.Append_File);
               exception
                  when Ada.Streams.Stream_IO.Use_Error =>
                     Report.Not_Applicable
                        ( "Reset to Append_File not supported " &
                          "for Stream_IO - 4" );
                     raise Incomplete;
               end Reset4;

               Test_Boolean :=
                 Ada.Streams.Stream_IO.End_Of_File (Stream_File_Object);
               Report.Failed
                 ("Exception not raised by EOF of Append file");
            exception
               when Incomplete =>
                  raise;
               when Ada.Streams.Stream_IO.Mode_Error =>
                  null;
               when others     =>
                  Report.Failed ("Incorrect exception raised - 5b");
            end EOF_For_Append_File;

         when others => Report.Failed ("Incorrect exception raised - 5a");
      end Check_End_File;



      -- In a similar situation to the above cases for attribute 'Read,
      -- an attempt to use the 'Output attribute on a stream file that
      -- is currently in In_File mode should result in Mode_Error being
      -- raised.
         Check_Output_Mode_Error:
         begin
            Reset5:
            begin
               Ada.Streams.Stream_IO.Reset (Stream_File_Object,
                                            Ada.Streams.Stream_IO.In_File);
         exception
            when Ada.Streams.Stream_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Stream_IO - 6" );
               raise Incomplete;
         end Reset5;

         Stream_Access_Value :=
           Ada.Streams.Stream_IO.Stream (Stream_File_Object);
         String'Output (Stream_Access_Value, "User-Oriented String");
         Report.Failed ("Exception not raised by 'Output to In_File");
      exception
         when Incomplete =>
            null;
         when Ada.Streams.Stream_IO.Mode_Error =>
            null;
         when others     =>
            Report.Failed ("Incorrect exception raised - 6");
      end Check_Output_Mode_Error;

      -- Any case of attempting to Open a stream file with a string for
      -- the parameter Name that does not allow the identification of an
      -- external file will result in the exception Name_Error being
      -- raised.
      Check_Illegal_File_Name:
      begin
         if Ada.Streams.Stream_IO.Is_Open (Stream_File_Object) then
            Ada.Streams.Stream_IO.Close (Stream_File_Object);
         end if;
         -- No external file exists with this filename, allowing no
         -- association with an internal file object, resulting in the
         -- raising of the exception Name_Error.
         Ada.Streams.Stream_IO.Open(File => Stream_File_Object,
                                    Mode => Ada.Streams.Stream_IO.Out_File,
                                    Name => Report.Legal_File_Name(2));
         Report.Failed ("Exception not raised by bad filename on Open");
      exception
         when Ada.Streams.Stream_IO.Name_Error =>
            null;
         when others     =>
            Report.Failed ("Incorrect exception raised - 7");
      end Check_Illegal_File_Name;

   exception
         when Incomplete =>
            null;
         when others =>
            Report.Failed ("Unexpected exception in Operational Test Block");

   end Operational_Test_Block;

   Deletion:
   begin
      if Ada.Streams.Stream_IO.Is_Open (Stream_File_Object) then
         Ada.Streams.Stream_IO.Delete (Stream_File_Object);
      else
         Ada.Streams.Stream_IO.Open (Stream_File_Object,
                                     Ada.Streams.Stream_IO.Out_File,
                                     Stream_Filename);
         Ada.Streams.Stream_IO.Delete (Stream_File_Object);
      end if;
   exception
      when others =>
         Report.Failed
            ( "Delete not properly implemented for Stream_IO" );
   end Deletion;

   Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAC003;
