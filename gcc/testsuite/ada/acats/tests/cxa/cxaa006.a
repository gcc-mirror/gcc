-- CXAA006.A
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
--      Check that for a bounded line length text file of mode Append_File,
--      when the number of characters to be output exceeds the number of 
--      columns remaining on the current line, a call to Put will output 
--      characters of the string sufficient to fill the remaining columns of 
--      the line (up to line length), then output a line terminator, reset the 
--      column number, increment the line number, then output the balance of 
--      the item.
--      
--      Check that the procedure Put does not raise Layout_Error when the 
--      number of characters to be output exceeds the line length of a bounded
--      text file of mode Append_File.
--      
-- TEST DESCRIPTION:
--      This test demonstrates the situation where an application intends to 
--      output variable length string elements to a text file in the most
--      efficient manner possible.  This is the case in a typesetting 
--      environment where text is compressed and split between lines of a 
--      bounded length.
--
--      The procedure Put will break string parameters placed in the file at
--      the point of the line length.  Two examples are demonstrated in this
--      test, one being the case where only one column remains on a line, and
--      the other being the case where a larger portion of the line remains
--      unfilled, but still not sufficient to contain the entire output
--      string.
--
--      During the course of the test, the file is reset to Append_File mode,
--      and the bounded line length is modified for different lines of the
--      file.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that support Text_IO
--      processing and external files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Ada.Text_IO;
with Report;

procedure CXAA006 is

   A_Bounded_File    : Ada.Text_IO.File_Type;
   Bounded_File_Name : constant String := 
                       Report.Legal_File_Name ( Nam => "CXAA006" );
   Incomplete : exception;

begin

   Report.Test ("CXAA006", "Check that procedure Put will correctly " &
                           "output string items to a bounded line "   &
                           "length text file of mode Append_File");

   Test_for_Text_IO_Support:
   begin

-- An application creates a text file in mode Append_File, with the intention 
-- of using the procedure Put to compress variable length string data into the 
-- file in the most efficient manner possible.  

     Ada.Text_IO.Create (File => A_Bounded_File,
                         Mode => Ada.Text_IO.Append_File,
                         Name => Bounded_File_Name);
   exception
      when Ada.Text_IO.Use_Error | Ada.Text_IO.Name_Error =>
         Report.Not_Applicable
            ( "Files not supported - Create with Append_File for Text_IO" );
         raise Incomplete;
   end Test_For_Text_IO_Support;

   Operational_Test_Block:
   declare
      Twelve_Characters   : constant String := "12Characters";
      Nineteen_Characters : constant String := "Nineteen_Characters";
      TC_Line             : Natural := 0;

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

-- The application sets the line length of the file to be bound at 20.  All
-- lines in this file will be limited to that length.

      Ada.Text_IO.Set_Line_Length (A_Bounded_File, 20);

      Ada.Text_IO.Put (A_Bounded_File, Nineteen_Characters);

      -- Test control code.
      if (Integer(Ada.Text_IO.Line (A_Bounded_File)) /=
          Report.Ident_Int(1))                         or
         (Integer(Ada.Text_IO.Col (A_Bounded_File))  /=
          Report.Ident_Int(20))                        then
         Report.Failed ("Incorrect position after 1st Put");
      end if;

-- The application finds that there is only one column available on the
-- current line, so the next string item to be output must be broken at 
-- the appropriate place (following the first character).

      Ada.Text_IO.Put (File => A_Bounded_File, 
                       Item => Twelve_Characters);

      -- Test control code.
      if (Integer(Ada.Text_IO.Line (A_Bounded_File)) /=
          Report.Ident_Int(2))                         or
         (Integer(Ada.Text_IO.Col (A_Bounded_File))  /=
          Report.Ident_Int(12))                        then
         Report.Failed ("Incorrect position after 2nd Put");
      end if;

-- The application subsequently modifies the processing, resetting the file
-- at this point to In_File mode in order to verify data that has been written 
-- to the file. Following this, the application resets the file to Append_File
-- mode in order to continue the placement of data into the file, but modifies
-- the original bounded line length for subsequent lines to be appended.

      -- Reset to Append mode; call outputs page terminator and
      -- resets line length to Unbounded.
      Reset1:
      begin
         Ada.Text_IO.Reset (A_Bounded_File, 
                            TC_Mode_Selection (Report.Ident_Int(3)));
      exception
         when Ada.Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to Append_File not supported for Text_IO" );
            raise Incomplete;
      end Reset1;

      Ada.Text_IO.Set_Line_Length (A_Bounded_File, 15);

      -- Store line number for later comparison.
      TC_Line := Natural(Ada.Text_IO.Line(A_Bounded_File));

-- The application finds that fifteen columns are available on the current
-- line but that the string item to be output exceeds this available space.
-- It must be split at the end of the line, and the balance placed on the
-- next file line.

      Ada.Text_IO.Put (File => A_Bounded_File, 
                       Item => Nineteen_Characters);

      -- Test control code.
      -- Positioned on new line at col 5.
      if (Natural(Ada.Text_IO.Line (A_Bounded_File)) /=
          (TC_Line + 1))                                 or
         (Integer(Ada.Text_IO.Col (A_Bounded_File))  /=
          Report.Ident_Int(5))                          then
         Report.Failed ("Incorrect position after 3rd Put");
      end if;


      Test_Verification_Block:                  
      declare                                   
         First_String  : String (1 .. 80);
         Second_String : String (1 .. 80);
         Third_String  : String (1 .. 80);
         Fourth_String : String (1 .. 80);
         TC_Width1     : Natural;
         TC_Width2     : Natural;
         TC_Width3     : Natural;
         TC_Width4     : Natural;
      begin  

-- The application has the capability to reset the file to In_File mode to
-- verify some or all of the data that is contained there.

         Reset2:
         begin
            Ada.Text_IO.Reset    (A_Bounded_File, Ada.Text_IO.In_File);
         exception
            when others =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset2;

         Ada.Text_IO.Get_Line 
            (A_Bounded_File, First_String,  TC_Width1);
         Ada.Text_IO.Get_Line 
            (A_Bounded_File, Second_String, TC_Width2);
         Ada.Text_IO.Get_Line 
            (A_Bounded_File, Third_String,  TC_Width3);
         Ada.Text_IO.Get_Line 
            (A_Bounded_File, Fourth_String, TC_Width4);

         -- Test control code.
         if (First_String (1..TC_Width1) /= Nineteen_Characters & "1")  or
            (Second_String (1..TC_Width2) /= "2Characters")             or
            (Third_String (1..TC_Width3)  /= 
             Nineteen_Characters(1..15))                                or
            (Fourth_String (1..TC_Width4) /= "ters")
         then
            Report.Failed ("Data placed incorrectly in file");
         end if;

      exception

         when Incomplete =>
            raise;

         when Ada.Text_IO.End_Error =>
            Report.Failed ("Incorrect number of lines in file");

         when others            => 
            Report.Failed ("Error raised during data verification");

      end Test_Verification_Block;

   exception

      when Ada.Text_IO.Layout_Error => 
         Report.Failed ("Layout Error raised when positioning text");

      when others               => 
         Report.Failed ("Exception in Text_IO processing");

   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Ada.Text_IO.Is_Open(A_Bounded_File) then
         Ada.Text_IO.Delete (A_Bounded_File);
      else
         Ada.Text_IO.Open (A_Bounded_File, 
                           Ada.Text_IO.In_File,
                           Bounded_File_Name);
         Ada.Text_IO.Delete (A_Bounded_File);
      end if;

   exception
      when others =>
         Report.Failed 
            ( "Delete not properly implemented for Text_IO" );
   end Final_Block;

   Report.Result;

exception

   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA006;
