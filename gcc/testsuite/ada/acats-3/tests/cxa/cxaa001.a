-- CXAA001.A
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
--      Check that the Line_Length and Page_Length maximums for a Text_IO
--      file of mode Append_File are initially zero (unbounded) after a
--      Create, Open, or Reset, and that these values can be modified using
--      the procedures Set_Line_Length and Set_Page_Length.
--      Check that setting the Line_Length and Page_Length attributes to zero
--      results in an unbounded Text_IO file.
--      Check that setting the line length when in Append_Mode doesn't 
--      change the length of lines previously written to the Text_IO file.
--
-- TEST DESCRIPTION:
--      This test attempts to simulate a possible text processing environment.
--      String values, from a number of different string types, are written to 
--      a Text_IO file.  Prior to the writing of each, the line length is set 
--      to the particular length of the data being written.  In addition, the 
--      default line and page lengths are checked, to determine whether they 
--      are unbounded (length = 0) following a create, reset, or open of a 
--      Text_IO file with mode Append_File.
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

procedure CXAA001 is
   use Ada;
   Data_File      : Text_IO.File_Type;
   Data_Filename  : constant String :=
                              Report.Legal_File_Name ( Nam => "CXAA001" );
   Incomplete : exception;
begin

   Report.Test ("CXAA001","Check that the Line_Length and Page_Length "      &
                          "maximums for a Text_IO file of mode Append_File " &
                          "are initially zero (unbounded) after a Create, "  &
                          "Open, or Reset, and that these values can be "    &
                          "modified using the procedures Set_Line_Length "   &
                          "and Set_Page_Length");

   Test_for_Text_IO_Support:
   begin

      -- An implementation that does not support Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise an exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Text_IO.Create (File => Data_File,
                      Mode => Text_IO.Append_File,
                      Name => Data_Filename);

   exception

       when Text_IO.Use_Error | Text_IO.Name_Error =>
          Report.Not_Applicable
             ( "Files not supported - Create as Append_File for Text_IO" );
          raise Incomplete;

   end Test_for_Text_IO_Support;

   Operational_Test_Block:
   declare

      subtype Confidential_Data_Type is string (1 .. 10);
      subtype Secret_Data_Type       is string (1 .. 20);
      subtype Top_Secret_Data_Type   is string (1 .. 30);

      Zero                   : constant Text_IO.Count :=  0;
      Confidential_Data_Size : constant Text_IO.Count := 10;
      Secret_Data_Size       : constant Text_IO.Count := 20;
      Top_Secret_Data_Size   : constant Text_IO.Count := 30;

      -- The following generic procedure is designed to simulate a text
      -- processing environment where line and page sizes are set and 
      -- verified prior to the writing of data to a file.

      generic
         Data_Size : Text_IO.Count;
      procedure Write_Data_To_File (Data_Item : in String);

      procedure Write_Data_To_File (Data_Item : in String) is
         use Text_IO;  -- Used to provide visibility to the "/=" operator.
      begin
         if (Text_IO.Line_Length (Data_File) /= Zero) then -- Check default
            Report.Failed("Line not of unbounded length"); -- line length,
         elsif (Text_IO.Page_Length (Data_File) /= Zero) then -- default
            Report.Failed ("Page not of unbounded length"); -- page length.
         end if;
      
         Text_IO.Set_Line_Length (File => Data_File,    -- Set the line
                                  To   => Data_Size);   -- length.
         Text_IO.Set_Page_Length (File => Data_File,    -- Set the page
                                  To   => Data_Size);   -- length.
         -- Verify the lengths set.
         if (Integer(Text_IO.Line_Length (Data_File)) /= 
             Report.Ident_Int(Integer(Data_Size))) then
            Report.Failed ("Line length not set to appropriate length");
         elsif (Integer(Text_IO.Page_Length (Data_File)) /= 
                Report.Ident_Int(Integer(Data_Size))) then
            Report.Failed ("Page length not set to appropriate length");
         end if;

         Text_IO.Put_Line (File => Data_File,           -- Write data to
                           Item => Data_Item);          -- file.

      end Write_Data_To_File;

      -- Instantiation for the three data types/sizes.

      procedure Write_Confidential_Data is 
        new Write_Data_To_File (Data_Size => Confidential_Data_Size);

      procedure Write_Secret_Data is 
        new Write_Data_To_File (Data_Size => Secret_Data_Size);

      procedure Write_Top_Secret_Data is 
        new Write_Data_To_File (Data_Size => Top_Secret_Data_Size);

      Confidential_Item : Confidential_Data_Type := "Confidenti";
      Secret_Item       : Secret_Data_Type       := "Secret Data Values  ";
      Top_Secret_Item   : Top_Secret_Data_Type   := 
        "Extremely Top Secret Data     ";

   begin

      -- The following call simulates processing occurring after the create
      -- of a Text_IO file with mode Append_File.

      Write_Confidential_Data (Confidential_Item);
    
      -- The following call simulates processing occurring after the reset
      -- of a Text_IO file with mode Append_File.

      Reset1:
      begin
         Text_IO.Reset (Data_File, Text_IO.Append_File); -- Reset to
                                                         -- Append_File mode.
      exception
         when Text_IO.Use_Error =>
            Report.Not_Applicable
               ( "Reset to Append_File not supported for Text_IO" );
            raise Incomplete;
      end Reset1;

      Write_Secret_Data (Data_Item    => Secret_Item);

      Text_IO.Close (Data_File);                      -- Close file.

      -- The following processing simulates processing occurring after the
      -- opening of an existing file with mode Append_File.

      Text_IO.Open (Data_File,                        -- Open file in
                    Text_IO.Append_File,              -- Append_File mode.
                    Data_Filename);  
                                                         
      Write_Top_Secret_Data (Top_Secret_Item); 

      Test_Verification_Block:
      declare
         TC_String1,
         TC_String2,
         TC_String3  : String (1..80) := (others => ' ');
         TC_Length1,
         TC_Length2,
         TC_Length3  : Natural := 0;
      begin

         Reset2:
         begin
            Text_IO.Reset (Data_File, Text_IO.In_File); -- Reset for reading.
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset2;

         Text_IO.Get_Line (Data_File, TC_String1, TC_Length1); 
         Text_IO.Get_Line (Data_File, TC_String2, TC_Length2); 
         Text_IO.Get_Line (Data_File, TC_String3, TC_Length3); 

         -- Verify that the line lengths of each line were accurate.
         -- Note: Each data line was written to the file after the
         -- particular line length had been set (to the data length).

         if not ((TC_Length1 = Natural(Confidential_Data_Size)) and 
                 (TC_Length2 = Natural(Secret_Data_Size))       and 
                 (TC_Length3 = Natural(Top_Secret_Data_Size)))  then
            Report.Failed ("Inaccurate line lengths read from file");
         end if;

         -- Verify that the data read from the file are accurate.
 
         if (TC_String1(1..TC_Length1) /= Confidential_Item) or else
            (TC_String2(1..TC_Length2) /= Secret_Item)       or else
            (TC_String3(1..TC_Length3) /= Top_Secret_Item)   then
            Report.Failed ("Corrupted data items read from file");
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

      Deletion:
      begin
         -- Check that the file is open prior to deleting it.
         if Text_IO.Is_Open(Data_File) then
            Text_IO.Delete(Data_File);
         else
            Text_IO.Open(Data_File, Text_IO.In_File, Data_Filename);
            Text_IO.Delete(Data_File);
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

end CXAA001;
