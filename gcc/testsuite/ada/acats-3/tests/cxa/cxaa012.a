-- CXAA012.A
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
--      Check that the exception Mode_Error is raised when an attempt is made
--      to read from (perform a Get_Line) or use the predefined End_Of_File 
--      function on a text file with mode Append_File.
--      
-- TEST DESCRIPTION:
--      A scenario is created that demonstrates the potential for the 
--      incorrect usage of predefined text processing subprograms, resulting 
--      from their use with files of the wrong Mode.  This results in the 
--      raising of Mode_Error exceptions, which is handled within blocks
--      embedded in the test.
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
--      27 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Ada.Text_IO;
with Report;

procedure CXAA012 is
   use Ada;
   Text_File     : Text_IO.File_Type;
   Text_Filename : constant String :=
                      Report.Legal_File_Name ( Nam => "CXAA012" );
   Incomplete : exception;
begin

        Report.Test ("CXAA012", "Check that the exception Mode_Error is " &
                                "raised when an attempt is made to read " &
                                "from (perform a Get_Line) or use the "   &
                                "predefined End_Of_File function on a "   &
                                "text file with mode Append_File");

        Test_for_Text_IO_Support:
        begin

        -- Use_Error or Name_Error will be raised if Text_IO operations 
        -- or external files are not supported.

           Text_IO.Create (Text_File, Text_IO.Out_File, Text_Filename);

      exception
          when Text_IO.Use_Error | Text_IO.Name_Error =>
             Report.Not_Applicable
                ( "Files not supported - Create as Out_File for Text_IO" );
             raise Incomplete;
      end Test_for_Text_IO_Support;

      -- The application writes some amount of data to the file.

           Text_IO.Put_Line (Text_File, "Data entered into the file");

           Text_IO.Close (Text_File);

           Operational_Test_Block:
           declare
              TC_Number_Of_Forced_Mode_Errors : constant Natural := 2;
              TC_Mode_Errors : Natural := 0;
           begin

              Text_IO.Open (Text_File, Text_IO.Append_File, Text_Filename);

              Test_for_Reading:
              declare
                 TC_Data   : String (1..80);
                 TC_Length : Natural := 0;
              begin

-- During the course of its processing, the application may become confused
-- and erroneously attempt to read data from the file that is currently in
-- Append_File mode (instead of the anticipated In_File mode).
-- This would result in the raising of Mode_Error.

                 Text_IO.Get_Line (Text_File, TC_Data, TC_Length);
                 Report.Failed ("Exception not raised by Get_Line");

-- An exception handler present within the application handles the exception
-- and processing can continue.

              exception
                 when Text_IO.Mode_Error => 
                    TC_Mode_Errors := TC_Mode_Errors + 1;
                 when others             => 
                    Report.Failed ("Exception in Get_Line processing");
              end Test_for_Reading;


              Test_for_End_Of_File:
              declare
                 TC_End_Of_File : Boolean;
              begin

-- Again, during the course of its processing, the application attempts to 
-- call the End_Of_File function for the file that is currently in
-- Append_File mode (instead of the anticipated In_File mode).

                 TC_End_Of_File := Text_IO.End_Of_File (Text_File);
                 Report.Failed ("Exception not raised by End_Of_File");

-- Once again, an exception handler present within the application handles 
-- the exception and processing continues.

              exception
                 when Text_IO.Mode_Error => 
                    TC_Mode_Errors := TC_Mode_Errors + 1;
                 when others             => 
                    Report.Failed("Exception in End_Of_File processing");
              end Test_for_End_Of_File;


              if (TC_Mode_Errors /= TC_Number_Of_Forced_Mode_Errors) then
                 Report.Failed ("Incorrect number of exceptions handled");
              end if;

           end Operational_Test_Block;
     
           -- Delete the external file.
           if Text_IO.Is_Open (Text_File) then
              Text_IO.Delete (Text_File);     
           else
              Text_IO.Open (Text_File, Text_IO.In_File, Text_Filename);
              Text_IO.Delete (Text_File);     
           end if;
         
        Report.Result;

exception
   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA012;
