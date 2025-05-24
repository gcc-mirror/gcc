-- CXAA005.A
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
--      Check that the procedure Put, when called with string parameters, does
--      not update the line number of a text file of mode Append_File, when
--      the line length is unbounded (i.e., only the column number is 
--      updated).
--      Check that a call to the procedure Put with a null string argument
--      has no measurable effect on a text file of mode Append_File.
--      
-- TEST DESCRIPTION:
--      This test is designed to ensure that when a string is appended to an
--      unbounded text file, it is placed following the last element currently
--      in the file.  For an unbounded text file written with Put procedures
--      only (not Put_Line), the line number should not be incremented by
--      subsequent calls to Put in Append_File mode.  Only the column number
--      should be incremented based on the length of the string parameter 
--      placed in the file.  If a call to Put with a null string argument is
--      made, no change to the line or column number should occur, and no
--      element(s) should be added to the file, so that there would be no 
--      measurable change to the file.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that support Text_IO
--      processing and external files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      24 Feb 97   CTA.PWB Allowed for non-support of some IO operations.
--!

with Ada.Text_IO;
with Report;

procedure CXAA005 is
   An_Unbounded_File   : Ada.Text_IO.File_Type;
   Unbounded_File_Name : constant String := 
                            Report.Legal_File_Name ( Nam => "CXAA005" ); 
   Incomplete          : exception;

begin

   Report.Test ("CXAA005", "Check that the procedure Put does not " &
                           "increment line numbers when used with " &
                           "unbounded text files of mode Append_File");

   Test_for_Text_IO_Support:
   begin

   -- An application creates a text file in mode Out_File, with the intention
   -- of entering string data packets into the file as appropriate.  In the 
   -- event that the particular environment where the application is running 
   -- does not support Text_IO, Use_Error will be raised on calls to Text_IO 
   -- operations.  
   -- This exception will be handled to produce a Not_Applicable result.

      Ada.Text_IO.Create (File => An_Unbounded_File,
                          Mode => Ada.Text_IO.Out_File,
                          Name => Unbounded_File_Name);
   exception
      when Ada.Text_IO.Use_Error | Ada.Text_IO.Name_Error =>
         Report.Not_Applicable
            ( "Files not supported - Create for Text_IO" );
         raise Incomplete;
   end Test_For_Text_IO_Support;

   Operational_Test_Block:
   declare
      subtype String_Sequence_Type is string (1 .. 20);
      type    String_Pointer_Type  is access String_Sequence_Type;

-- During the course of processing, the application creates a variety of data
-- pointers that refer to particular data items.  The possibility of having
-- null data values in this environment exists.

      Data_Packet_1     : String_Pointer_Type := 
        new String_Sequence_Type'("One Data Sequence 01");

      Data_Packet_2     : String_Pointer_Type :=
        new String_Sequence_Type'("New Data Sequence 02");
          
      Blank_Data_Packet : String_Pointer_Type :=
        new String_Sequence_Type'("                    ");

      Null_Data_Packet  : constant String     := "";

      TC_Line, TC_Col   : Natural := 0;

      function TC_Mode_Selection (Selector : Integer) 
         return Ada.Text_IO.File_Mode is
      begin
         case Selector is
            when 1       => return Ada.Text_IO.In_File;
            when 2       => return Ada.Text_IO.Out_File;
            when others  => return Ada.Text_IO.Append_File;
         end case;
      end TC_Mode_Selection;

   begin                                         

-- The application places some data into the file, using the Put subroutine.
-- This operation can occur one-to-many times.

       Ada.Text_IO.Put (An_Unbounded_File, Data_Packet_1.all);
   
       -- Test control code.
       if (Integer(Ada.Text_IO.Col (An_Unbounded_File))  /=
           Report.Ident_Int(21))                            or
          (Integer(Ada.Text_IO.Line (An_Unbounded_File)) /=
           Report.Ident_Int(1))                             then
          Report.Failed ("Incorrect Col position after 1st Put");
       end if;

-- The application may close the file at some point following its initial 
-- entry of data.

      Ada.Text_IO.Close (An_Unbounded_File);

-- At some later point in the processing, more data needs to be added to the
-- file, so the application opens the file in Append_File mode.

      Ada.Text_IO.Open (File => An_Unbounded_File,
                        Mode => Ada.Text_IO.Append_File,
                        Name => Unbounded_File_Name);

      -- Test control code.
      -- Store line/column number for later comparison.
      TC_Line := Natural(Ada.Text_IO.Line(An_Unbounded_File));
      TC_Col  := Natural(Ada.Text_IO.Col(An_Unbounded_File));

-- Additional data items can then be appended to the file.

      Ada.Text_IO.Put (An_Unbounded_File, Blank_Data_Packet.all);

      -- Test control code.
      if (Natural(Ada.Text_IO.Col (An_Unbounded_File))  /=
          (TC_Col + 20))                                   or
         (Natural(Ada.Text_IO.Line (An_Unbounded_File)) /=
          TC_Line)                                         then
         Report.Failed ("Incorrect Col position after 2nd Put");
      end if;

-- In order to accommodate various scenarios, the application may have changed
-- the mode of the data file to In_File in order to retrieve/verify some of 
-- the data contained there.  However, with the need to place more data into 
-- the file, the file can be reset to Append_File mode.

      Reset1:
      begin
         Ada.Text_IO.Reset (An_Unbounded_File, 
                            TC_Mode_Selection (Report.Ident_Int(3)));
      exception
         when Ada.Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to Append_File not supported for Text_IO" );
            raise Incomplete;
      end Reset1;

      -- Test control code.
      -- Store line/column number for later comparison.
      TC_Line := Natural(Ada.Text_IO.Line(An_Unbounded_File));
      TC_Col  := Natural(Ada.Text_IO.Col(An_Unbounded_File));

-- Additional data can then be appended to the file.  On some occasions, an
-- attempt to enter a null string value into the file may occur.  This should
-- have no effect on the file, leaving it unchanged.

      -- No measurable effect from Put with null string.
      Ada.Text_IO.Put (An_Unbounded_File, Null_Data_Packet);

      -- Test control code.
      -- There should be no change following the Put above.
      if (Natural(Ada.Text_IO.Col (An_Unbounded_File))  /= 
          TC_Col)                                          or
         (Natural(Ada.Text_IO.Line (An_Unbounded_File)) /= 
          TC_Line)                                         then
         Report.Failed ("Incorrect Col position after 3rd Put");
      end if;

-- Additional data can be appended to the file.

      Ada.Text_IO.Put (An_Unbounded_File, Data_Packet_2.all);

      -- Test control code.
      if (Natural(Ada.Text_IO.Col (An_Unbounded_File))  /=
          (TC_Col + 20))                                    or
         (Integer(Ada.Text_IO.Line (An_Unbounded_File)) /=
          TC_Line)                                         then
         Report.Failed ("Incorrect Col position after 4th Put");
      end if;

      Test_Verification_Block:                  
      declare                                   
         File_Data : String (1 .. 80);
         TC_Width  : Natural;
      begin                                                
   
-- The application has the capability to reset the file to In_File mode to
-- verify some of the data that is contained there.

      Reset2:
      begin
         Ada.Text_IO.Reset (An_Unbounded_File, Ada.Text_IO.In_File);
      exception
         when Ada.Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to In_File not supported - Text_IO" );
            raise Incomplete;
      end Reset2;

      Ada.Text_IO.Get_Line (An_Unbounded_File,
                            File_Data, 
                            TC_Width);

      -- Test control code.
      -- Since it is implementation defined whether a page
      -- terminator separates preexisting text from new text 
      -- following an open in append mode (as occurred above), 
      -- verify only that the first data item written to the
      -- file was not overwritten by any subsequent call to Put.

      if (File_Data (File_Data'First) /= 'O') or
          (File_Data (20)              /= '1') then
         Report.Failed ("Data placed incorrectly in file");
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
         Report.Failed ("Exception in Text_IO processing");
   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Ada.Text_IO.Is_Open(An_Unbounded_File) then
         Ada.Text_IO.Delete (An_Unbounded_File);
      else
         Ada.Text_IO.Open(An_Unbounded_File, 
                          Ada.Text_IO.In_File,
                          Unbounded_File_Name);
         Ada.Text_IO.Delete (An_Unbounded_File);
      end if;
   exception
      when others =>
         Report.Failed
            ( "Delete not properly implemented -- Text_IO" );
   end Final_Block;

   Report.Result;

exception

   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA005;
