-- CXAA008.A
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
--      Check that the capabilities provided in instantiations of the 
--      Ada.Text_IO.Fixed_IO package operate correctly when the mode of
--      the file is Append_File.  Check that Fixed_IO procedures Put and Get 
--      properly transfer fixed point data to/from data files that are in
--      Append_File mode.  Check that the formatting parameters available in 
--      the package can be used and modified successfully in the appending and 
--      retrieval of data.
--      
-- TEST DESCRIPTION:
--      This test simulates order processing, with data values being written
--      to a file, in a specific format, using Fixed_IO.  Validation is done
--      on this process by reading the data values from the file, and 
--      comparing them for equality with the values originally written to 
--      the file.
--      
--      This test verifies issues of create in Append_File mode, appending to
--      a file previously appended to, resetting to Append_File mode, 
--      resetting from Append_File mode to In_File mode, as well as a 
--      variety of Text_IO and Fixed_IO predefined subprograms.
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

procedure CXAA008 is
   use Ada;

   Inventory_File     : Text_IO.File_Type;
   Inventory_Filename : constant String :=
                          Report.Legal_File_Name ( Nam => "CXAA008" );
   Incomplete         : exception;

begin

   Report.Test ("CXAA008", "Check that the capabilities of "               &
                           "Text_IO.Fixed_IO operate correctly for files " &
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
   end Test_For_Text_IO_Support;

   Operational_Test_Block:
   declare

      Daily_Orders_Received : constant Natural := 4;

      type Item_Type   is delta 0.1  range    0.0 .. 5000.0;
      type Cost_Type   is delta 0.01 range    0.0 .. 10_000.0;
      type Profit_Type is delta 0.01 range -100.0 .. 1000.0;

      type Product_Type is record
         Item_Number    : Item_Type   := 0.0;
         Unit_Cost      : Cost_Type   := 0.00;
         Percent_Markup : Profit_Type := 0.00;
      end record;   

      type Inventory_Type is 
        array (1 .. Daily_Orders_Received) of Product_Type;

      Daily_Inventory   : Inventory_Type := ((   1.0,   1.75,  50.00),  
                                             ( 155.0,  20.00,  -5.50),  
                                             (3343.5,   2.50, 126.50),
                                             (4986.0, 180.00,  31.75));

      package Item_IO   is new Text_IO.Fixed_IO (Item_Type);
      package Cost_IO   is new Text_IO.Fixed_IO (Cost_Type);
      package Markup_IO is new Text_IO.Fixed_IO (Profit_Type);


      function TC_Mode_Selection (Selector : Integer) 
        return Text_IO.File_Mode is
      begin
         case Selector is
            when 1       => return Text_IO.In_File;
            when 2       => return Text_IO.Out_File;
            when others  => return Text_IO.Append_File;
         end case;
      end TC_Mode_Selection;


      -- The following function simulates the addition of inventory item 
      -- information into a data file.  Boolean status of True is returned
      -- if all of the data entry was successful, False otherwise.

      function Update_Inventory (The_List : Inventory_Type) 
        return Boolean is
      begin
         for I in 1 .. Daily_Orders_Received loop
            Item_IO.Put  (Inventory_File, The_List(I).Item_Number);    
            Cost_IO.Put  (Inventory_File, The_List(I).Unit_Cost, 10, 4, 0);
            Markup_IO.Put(File => Inventory_File, 
                          Item => The_List(I).Percent_Markup,
                          Fore => 6,
                          Aft  => 3,
                          Exp  => 2);
            Text_IO.New_Line (Inventory_File);
         end loop;
         return (True);                         -- Return a Status value.
      exception
         when others => return False;
      end Update_Inventory;


   begin

      -- This code section simulates a receiving department maintaining a
      -- data file containing information on items that have been ordered
      -- and received. 

      -- Whenever items are received, the file is reset to Append_File
      -- mode.  Data is taken from an inventory list and entered into the
      -- file, in specific format.  

      Reset1:
      begin                                                -- Reset to 
         Text_IO.Reset (Inventory_File,                    -- Append mode.
                        TC_Mode_Selection (Report.Ident_Int(3))); 
      exception
         when Text_IO.Use_Error =>
            Report.Not_Applicable 
               ( "Reset to Append_File not supported for Text_IO" );
      end Reset1;

                                                           -- Enter data.
      if not Update_Inventory (The_List => Daily_Inventory) then
         Report.Failed ("Exception occurred during inventory update");
         raise Incomplete;
      end if;

      Test_Verification_Block:                             
      declare                                              
         TC_Item       : Item_Type;
         TC_Cost       : Cost_Type;
         TC_Markup     : Profit_Type;
         TC_Item_Count : Natural := 0;                     
      begin                                                

         Reset2:
         begin
            Text_IO.Reset (Inventory_File, Text_IO.In_File);  -- Reset for
                                                              -- reading.
         exception
            when Text_IO.Use_Error =>
               Report.Not_Applicable
                  ( "Reset to In_File not supported for Text_IO" );
               raise Incomplete;
         end Reset2;

         while not Text_IO.End_Of_File (Inventory_File) loop
            Item_IO.Get   (Inventory_File, TC_Item);    
            Cost_IO.Get   (Inventory_File, TC_Cost);
            Markup_IO.Get (File  => Inventory_File, 
                           Item  => TC_Markup,
                           Width => 0);
            Text_IO.Skip_Line (Inventory_File);
            TC_Item_Count := TC_Item_Count + 1;

            -- Verify all of the data fields read from the file.  Compare
            -- with the values that were originally entered into the file.

            if (TC_Item /= Daily_Inventory(TC_Item_Count).Item_Number) then
               Report.Failed ("Error in Item_Number read from file");
            end if;
            if (TC_Cost /= Daily_Inventory(TC_Item_Count).Unit_Cost) then
               Report.Failed ("Error in Unit_Cost read from file");
            end if;
            if not (TC_Markup = 
                    Daily_Inventory(TC_Item_Count).Percent_Markup) then
               Report.Failed ("Error in Percent_Markup read from file");
            end if;

         end loop;

         if (TC_Item_Count /= Daily_Orders_Received) then
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
         Report.Failed ("Exception in Text_IO.Fixed_IO processing");
   end Operational_Test_Block;

   Final_Block:
   begin
      -- Delete the external file.
      if Text_IO.Is_Open (Inventory_File) then
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

end CXAA008;
