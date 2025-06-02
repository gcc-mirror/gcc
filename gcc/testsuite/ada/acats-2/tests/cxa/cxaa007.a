-- CXAA007.A
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
--      Check that the capabilities of Text_IO.Integer_IO perform correctly
--      on files of Append_File mode, for instantiations with integer and
--      user-defined subtypes.
--      Check that the formatting parameters available in the package can
--      be used and modified successfully in the storage and retrieval of
--      data.
--      
-- TEST DESCRIPTION:
--      This test simulates a receiving department inventory system.  Data on
--      items received is entered into an inventory database. This information
--      consists of integer entry number, item number, and bar code.  
--      One item is placed into the inventory file immediately following file
--      creation, subsequent items are entered following file opening in 
--      Append_File mode.  Data items are validated by reading all data from
--      the file and comparing against known values (those used to enter the
--      data originally).  
--      
--      This test verifies issues of create in Append_File mode, appending to
--      a file previously appended to, opening in Append_File mode, resetting
--      from Append_File mode to In_File mode, as well as a variety of Text_IO
--      and Integer_IO predefined subprograms.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable only to implementations that support text
--      files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Feb 97   PWB.CTA Allowed for non-support of some IO operations
--!

with Ada.Text_IO;
with Report;

procedure CXAA007 is
   use Ada;

   Inventory_File     : Text_IO.File_Type;
   Inventory_Filename : constant String :=
                           Report.Legal_File_Name ( Nam => "CXAA007" );
   Incomplete         : exception;

begin

   Report.Test ("CXAA007", "Check that the capabilities of "                 &
                           "Text_IO.Integer_IO operate correctly for files " &
                           "with mode Append_File");

   Test_for_Text_IO_Support:
   begin

      -- An implementation that does not support Text_IO in a particular
      -- environment will raise Use_Error on calls to various
      -- Text_IO operations.  This block statement encloses a call to
      -- Create, which should raise the exception in a non-supportive 
      -- environment.  This exception will be handled to produce a
      -- Not_Applicable result.

      Text_IO.Create (File => Inventory_File,
                      Mode => Text_IO.Append_File,
                      Name => Inventory_Filename);
   exception
      when Text_IO.Use_Error | Text_IO.Name_Error =>
         Report.Not_Applicable
            ( "Files not supported - Create with Append_File for Text_IO" );
         raise Incomplete;
   end Test_for_Text_IO_Support;

   Operational_Test_Block:
   declare

      Max_Entries_Per_Order : constant Natural := 4;

      type Bar_Code_Type is range 0 .. 127; -- Values to be stored as base
                                            -- two numbers in file.
      type Item_Type is record
         Entry_Number : Natural        := 0;
         Item_Number  : Integer        := 0;
         Bar_Code     : Bar_Code_Type  := 0;
      end record;   

      type Inventory_Type is 
        array (1 .. Max_Entries_Per_Order) of Item_Type;

      Inventory_List : Inventory_Type := ((1, 119,  87), -- Items received
                                          (2, 206,  44), -- this order.
                                          (3, -25, 126),
                                          (4, -18, 31));

      Daily_Order        : constant := 1;
      Entry_Field_Width  : constant Natural :=  1;
      Item_Base          : constant Natural := 16;
      Items_Inventoried  : Natural := 1;
      Items_To_Inventory : Natural := 4;

      package Entry_IO    is new Text_IO.Integer_IO (Natural);
      package Item_IO     is new Text_IO.Integer_IO (Integer);
      package Bar_Code_IO is new Text_IO.Integer_IO (Bar_Code_Type);


      -- The following procedure simulates the addition of inventory item 
      -- information into a data file.

      procedure Update_Inventory (The_Item : in Item_Type) is
         Spacer : constant String := "   ";
      begin
         -- Enter all the incoming data into the inventory file.
         Entry_IO.Put   (Inventory_File, The_Item.Entry_Number);
         Text_IO.Put    (Inventory_File, Spacer);
         Item_IO.Put    (Inventory_File, The_Item.Item_Number); 
         Text_IO.Put    (Inventory_File, Spacer);
         Bar_Code_IO.Put(File  => Inventory_File, 
                         Item  => The_Item.Bar_Code,
                         Width => 13,
                         Base  =>  2);
         Text_IO.New_Line(Inventory_File);
      end Update_Inventory;


   begin

      -- This code section simulates a receiving department maintaining a
      -- data file containing information on items that have been ordered
      -- and received.  
      --
      -- As new orders are received, the file is opened in Append_File
      -- mode.
      -- Data is taken from the inventory list and entered into the file, 
      -- in specific format.
      -- Enter the order into the inventory file.  This is item 1 in
      -- the inventory list.  
      -- The data entry process can be repeated numerous times as required.

      Entry_IO.Put     (Inventory_File, 
                        Inventory_List(Daily_Order).Entry_Number);    
      Item_IO.Put      (Inventory_File, 
                        Inventory_List(Daily_Order).Item_Number);
      Bar_Code_IO.Put  (File  => Inventory_File, 
                        Item  => Inventory_List(Daily_Order).Bar_Code); 
      Text_IO.New_Line (Inventory_File);
     
      Text_IO.Close    (Inventory_File);


      Entry_IO.Default_Width := Entry_Field_Width;  -- Modify the default
                                                    -- width of Entry_IO.
      Item_IO.Default_Base   := Item_Base;          -- Modify the default
                                                    -- number base of
                                                    -- Item_IO
      Text_IO.Open (Inventory_File, 
                    Text_IO.Append_File,            -- Open in Append mode.
                    Inventory_Filename);
                                                          -- Enter items
      while (Items_Inventoried < Items_To_Inventory) loop -- 2-4 into the
         Items_Inventoried := Items_Inventoried + 1;     -- inventory file.
         Update_Inventory (The_Item => Inventory_List (Items_Inventoried));
      end loop;

      Test_Verification_Block:                           -- Read and check
      declare                                            -- all the data
         TC_Entry      : Natural;                        -- values that
         TC_Item       : Integer;                        -- have been
         TC_Bar_Code   : Bar_Code_Type;                  -- entered in the
         TC_Item_Count : Natural := 0;                   -- data file.
      begin                                                

         Reset1:
         begin
            Text_IO.Reset (Inventory_File, Text_IO.In_File);  -- Reset for
                                                              -- reading.
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to mode In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset1;

         while not Text_IO.End_Of_File (Inventory_File) loop
            Entry_IO.Get      (Inventory_File, TC_Entry);
            Item_IO.Get       (Inventory_File, TC_Item);
            Bar_Code_IO.Get   (Inventory_File, TC_Bar_Code);
            Text_IO.Skip_Line (Inventory_File);
            TC_Item_Count := TC_Item_Count + 1;

            if (TC_Item     /= Inventory_List(TC_Entry).Item_Number) or
               (TC_Bar_Code /= Inventory_List(TC_Entry).Bar_Code)    then
               Report.Failed ("Error in integer data read from file");
            end if;
         end loop;

         if (TC_Item_Count /= Max_Entries_Per_Order) then
            Report.Failed ("Incorrect number of records read from file");
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
         Report.Failed ("Exception in Text_IO.Integer_IO processing");
   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Text_IO.Is_Open(Inventory_File) then
         Text_IO.Delete (Inventory_File);      
      else
         Text_IO.Open (Inventory_File, Text_IO.In_File, Inventory_Filename);
         Text_IO.Delete (Inventory_File);
      end if;

   exception

      when others =>
         Report.Failed ( "Delete not properly implemented for Text_IO" );

   end Final_Block;

   Report.Result;

exception

   when Incomplete =>
      Report.Result;
   when others     =>
      Report.Failed ( "Unexpected exception" );
      Report.Result;

end CXAA007;
