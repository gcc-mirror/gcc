-- CXAA004.A
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
--      Check that the procedures New_Page, Set_Line, Set_Col, and New_Line
--      perform properly on a text file opened with mode Append_File.
--      Check that the attributes Page, Line, and Column are all set to 1
--      following the opening of a text file with mode Append_File.
--      Check that the functions Page, Line, and Col perform properly on a
--      text file opened with mode Append_File.
--      Check that the procedures Put and Put_Line perform properly on text
--      files opened with mode Append_File.
--      Check that the procedure Set_Line sets the current line number to
--      the value specified by the parameter "To" for text files opened with
--      mode Append_File. 
--      Check that the procedure Set_Col sets the current column number to
--      the value specified by the parameter "To" for text files reset with
--      mode Append_File.
--
-- TEST DESCRIPTION:
--      This test is designed to simulate the text processing that could
--      occur with files that have been created in Out_File mode,
--      and then reset to Append_File mode. 
--      Various calls to Text_IO formatting subprograms are called to properly
--      position text appended to a document.  The text content and position
--      are subsequently verified for accuracy.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable only to implementations that support text
--      files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      24 Feb 97   PWB.CTA Allowed for non-support of some IO operations.
--!

with Ada.Text_IO;
with Report;

procedure CXAA004 is
   use Ada;   
   Data_File         : Text_IO.File_Type;
   Data_Filename     : constant String :=
                          Report.Legal_File_Name ( Nam => "CXAA004" );
   Incomplete        : exception;

begin

   Report.Test ("CXAA004", "Check that page, line, and column formatting " &
                           "subprograms perform properly on text files "   &
                           "opened with mode Append_File");

   Test_for_Text_IO_Support:
   begin

      -- An implementation that does not support Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise the exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Text_IO.Create (File => Data_File,
                      Mode => Text_IO.Out_File,
                      Name => Data_Filename);

   exception
      when Text_IO.Use_Error | Text_IO.Name_Error =>
         Report.Not_Applicable
            ( "Files not supported - Create for Text_IO" );
         raise Incomplete;
   end Test_for_Text_IO_Support;

   Operational_Test_Block:
   declare
      use Text_IO;  -- To provide visibility to the "/=" operator.

      Default_Position  : constant Text_IO.Positive_Count := 1;

      Section_Header    : constant String := "X.  ";
      Reference_Title   : constant String := "REFERENCES";
      Reference_Content : constant String := "Available Upon Request";

   begin

      -- Some amount of text processing would occur here in the scenario
      -- following file creation, prior to file closure.
      Text_IO.Put_Line (File => Data_File, Item => "Some optional data");

      -- Close has the effect of a call to New_Page (adding a page
      -- terminator).  
      Text_IO.Close (Data_File);

      -- This code section simulates a scenario that could occur in a 
      -- text processing environment:
      --     Certain text is to be appended to a document. 
      --     The file is opened in Append_File mode.
      --     The position on the appended page is set, verified, and text 
      --     is placed in the file.  
      --
      -- Note: The text file has been originally created in Out_File
      -- mode, has been subsequently closed and is now being reopened in
      -- Append_File mode for further processing.

         Text_IO.Open   (Data_File, Text_IO.Append_File, Data_Filename);

      -- Test control code.
      if (Text_IO.Page(Data_File) /= Default_Position) then -- Verify init.
         Report.Failed ("Incorrect default page number");   -- page value.
      end if;
      if (Text_IO.Line(Data_File) /= Default_Position) then -- Verify init.
         Report.Failed ("Incorrect default line number");   -- line number.
      end if;   
      if (Text_IO.Col (Data_File) /= Default_Position) then -- Verify init.
         Report.Failed ("Incorrect default column number"); -- column no.
      end if;   

      -- Simulated usage code.
      Text_IO.New_Page (Data_File);                        -- Set new page/
      Text_IO.New_Line (File => Data_File, Spacing => 2);  -- line pos.
      Text_IO.Put      (Data_File, Section_Header);        -- Position  
      Text_IO.Put_Line (Data_File, Reference_Title);       -- title.

      -- Test control code.                                -- Verify new
      if (Integer(Text_IO.Page (Data_File)) /=             -- page and
          Report.Ident_Int(2)) or else                     -- line.
         (Integer(Text_IO.Line (Data_File)) /= 
          Report.Ident_Int(4)) then                  
         Report.Failed ("Incorrect results from page/line positioning");
      end if;

      -- Simulated usage code.                               
      Text_IO.Set_Line (File => Data_File, To => 8);       -- Set new 
      Text_IO.Set_Col  (File => Data_File, To => 30);      -- position.
      Text_IO.Put_Line (Data_File, Reference_Content);      
                                                             
      -- Test control code.
      if (Integer(Text_IO.Line (Data_File)) /= 
          Report.Ident_Int(9))                   or         -- Verify new 
         (Integer(Text_IO.Col  (Data_File)) /=              -- position.
          Report.Ident_Int(1))                   then         
         Report.Failed ("Incorrect results from line/column positioning");
      end if;

      Test_Verification_Block:
      declare
         TC_Page, TC_Line, TC_Column : Text_IO.Positive_Count;
         TC_Position                 : Natural := 0;
         TC_String                   : String (1 .. 55) := (others => ' ');
      begin                                          

         Reset1:
         begin
            Text_IO.Reset     (Data_File, Text_IO.In_File);
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset1;

         Text_IO.Skip_Page (Data_File);

         -- If the Reset to Append_File mode actually put a page terminator
         -- in the file, as allowed (but not required) by RM A.10.2(4), then
         -- we are now on page 2, an empty page.  Therefore, we need to skip
         -- one more page.

         if Text_IO.End_Of_Page (Data_File) then
            Text_IO.Skip_Page (Data_File);
         end if;

         -- Now we're on the reference page.

                                                 -- Loop to the third line
         for I in 1 .. 3 loop                    -- and read the contents.
            Text_IO.Get_Line (Data_File, TC_String, TC_Position); 
         end loop;

         if (TC_Position /= 14) or else          -- Verify the title line.
            (TC_String (1..6)  /= "X.  RE") or else             
            (TC_String (2..14) /= (".  " & Reference_Title)) then
            Report.Failed ("Incorrect positioning of title line");
         end if;
                                                 -- Loop to the eighth line
         for I in 4 .. 8 loop                    -- and read the contents.
            Text_IO.Get_Line (Data_File, TC_String, TC_Position); 
         end loop;

         if (TC_Position /= 51) or               -- Verify the contents.
            (TC_String (30..51) /= "Available Upon Request") then     
            Report.Failed ("Incorrect positioning of contents line");
         end if;

      exception

            when Incomplete =>
               raise;      
            when others => 
               Report.Failed ("Error raised during data verification");

      end Test_Verification_Block;

   exception

      when Incomplete =>
         raise;
      when others => 
         Report.Failed ("Exception raised during Text_IO processing");

   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Text_IO.Is_Open (Data_File) then
         Text_IO.Delete (Data_File);        
      else
         Text_IO.Open (Data_File, Text_IO.In_File, Data_Filename);
         Text_IO.Delete (Data_File);        
      end if;
   exception
      when others =>
         Report.Failed ( "Delete not properly implemented - Text_IO" );
   end Final_Block;

   Report.Result;

exception

   when Incomplete =>
      Report.Result;
   when others =>
      Report.Failed ("Unexpected exception");
      Report.Result;

end CXAA004;
