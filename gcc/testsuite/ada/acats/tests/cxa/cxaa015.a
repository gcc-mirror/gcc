-- CXAA015.A
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
--      Check that the exception Status_Error is raised when an attempt is
--      made to create or open a file in Append_File mode when the file is 
--      already open.
--      Check that the exception Name_Error is raised by procedure Open when
--      attempting to open a file in Append_File mode when the name supplied
--      as the filename does not correspond to an existing external file.
--      
-- TEST DESCRIPTION:
--      A scenario is created that demonstrates the potential for the 
--      inappropriate usage of text processing subprograms Create and Open, 
--      resulting in the raising of Status_Error and Name_Error exceptions.
--      A count is kept to ensure that each anticipated exception is in fact
--      raised and handled properly.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable only to implementations that support text
--      files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      28 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Ada.Text_IO;
with Report;

procedure CXAA015 is
   use Ada;
   Text_File     : Text_IO.File_Type;
   Text_Filename : constant String := 
                              Report.Legal_File_Name ( Nam => "CXAA015" );
   Incomplete : exception;

begin

         Report.Test ("CXAA015", "Check that the appropriate exceptions "    &
                                 "are raised when procedures Create and "    &
                                 "Open are used to inappropriately operate " &
                                 "on files of mode Append_File");

         Test_for_Text_IO_Support:
         begin

-- An application creates a text file with mode Append_File.
-- Use_Error will be raised if Text_IO operations or external files are not
-- supported.

            Text_IO.Create (Text_File, Text_IO.Append_File, Text_Filename);
         exception

             when Text_IO.Use_Error | Text_IO.Name_Error =>
                Report.Not_Applicable
                   ( "Files not supported - Create as Append_File for Text_IO" );
                raise Incomplete;

         end Test_for_Text_IO_Support;


-- The application writes some amount of data to the file.

            for I in 1 .. 5 loop
               Text_IO.Put_Line (Text_File, "Data entered into the file");
            end loop;

            Operational_Test_Block:
            declare
               TC_Number_Of_Forced_Errors : constant Natural := 3;
               TC_Errors                  :          Natural := 0;
            begin


               Test_for_Create:
               begin

-- During the course of its processing, the application may (erroneously) 
-- attempt to create the same file already in existence in Append_File mode.  
-- This results in the raising of Status_Error.

                  Text_IO.Create (Text_File, 
                                  Text_IO.Append_File, 
                                  Text_Filename);
                  Report.Failed ("Exception not raised by Create");

-- An exception handler present within the application handles the exception
-- and processing can continue.

               exception
                  when Text_IO.Status_Error => 
                     TC_Errors := TC_Errors + 1;
                  when others => 
                     Report.Failed("Exception in Create processing");
               end Test_for_Create;


               First_Test_For_Open:
               begin

-- Again, during the course of its processing, the application incorrectly
-- attempts to Open a file (in Append_File mode) that is already open.

                  Text_IO.Open (Text_File, Text_IO.Append_File, Text_Filename);
                  Report.Failed ("Exception not raised by improper Open - 1");

-- Once again, an exception handler present within the application handles 
-- the exception and processing continues.

               exception
                  when Text_IO.Status_Error => 
                     TC_Errors := TC_Errors + 1;

-- At some point in its processing, the application closes the file that is
-- currently open.

                     Text_IO.Close (Text_File);
                  when others => 
                     Report.Failed("Exception in Open processing - 1");
               end First_Test_For_Open;


               Open_With_Wrong_Filename:
               declare
                  TC_Wrong_Filename : constant String := 
                    Report.Legal_File_Name(2);
               begin

-- At this point, the application attempts to Open (in Append_File mode) the
-- file used in previous processing, but it attempts this Open using a name 
-- string that does not correspond to any existing external file.
-- First make sure the file doesn't exist.  (If it did, then the check
-- for open in append mode wouldn't work.)

                  Verify_No_File:
                  begin
                     Text_IO.Open (Text_File,
                                   Text_IO.In_File,
                                   TC_Wrong_Filename);
                  exception
                     when Text_IO.Name_Error =>
                        null;
                     when others =>
                        Report.Failed ( "Unexpected exception on Open check" );
                  end Verify_No_File;

                  Delete_No_File:
                  begin
                     if Text_IO.Is_Open (Text_File) then
                        Text_IO.Delete (Text_File);
                     end if;
                  exception
                     when others =>
                       Report.Failed ( "Unexpected exception - Delete check" );
                  end Delete_No_File;

                  Text_IO.Open (Text_File, 
                                Text_IO.Append_File, 
                                TC_Wrong_Filename);
                  Report.Failed ("Exception not raised by improper Open - 2");

-- An exception handler for the Name_Error, present within the application,
-- catches the exception and processing continues.

               exception
                  when Text_IO.Name_Error => 
                     TC_Errors := TC_Errors + 1;
                  when others => 
                     Report.Failed("Exception in Open processing - 2");
               end Open_With_Wrong_Filename;


               if (TC_Errors /= TC_Number_Of_Forced_Errors) then
                  Report.Failed ("Incorrect number of exceptions handled");
               end if;

            end Operational_Test_Block;

            Deletion:
            begin
               -- Delete the external file.
               if Text_IO.Is_Open (Text_File) then
                  Text_IO.Delete (Text_File);     
               else
                  Text_IO.Open (Text_File, Text_IO.In_File, Text_Filename);
                  Text_IO.Delete (Text_File);     
               end if;
            exception
               when others =>
                  Report.Failed
                     ( "Delete not properly implemented for Text_IO" );
            end Deletion;

         Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA015;
