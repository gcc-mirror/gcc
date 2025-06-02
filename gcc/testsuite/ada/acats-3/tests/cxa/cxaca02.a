-- CXACA02.A
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
--      Check that user defined subprograms can override the default
--      attributes 'Read and 'Write using attribute definition clauses.
--      Use objects of record types. 
--
-- TEST DESCRIPTION:
--      This test demonstrates that the default implementations of the
--      'Read and 'Write attributes can be overridden by user specified 
--      subprograms in conjunction with attribute definition clauses.
--      These attributes have been overridden below, and in the user defined
--      substitutes, values are added or subtracted to global variables.
--      The global variables are evaluated to ensure that the user defined 
--      subprograms were used in overriding the type-related default 
--      attributes.
--      
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support external
--      Stream_IO files.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      21 Nov 95   SAIC    Corrected recursive attribute definitions 
--                          for ACVC 2.0.1.
--      24 Aug 96   SAIC    Corrected typo in test verification criteria.
--
--!

with Report;
with Ada.Streams.Stream_IO;

procedure CXACA02 is
begin

   Report.Test ("CXACA02", "Check that user defined subprograms can "   &
                           "override the default attributes 'Read and " &
                           "'Write using attribute definition clauses");

   Test_for_Stream_IO_Support:
   declare

      Data_File      : Ada.Streams.Stream_IO.File_Type;
      Data_Stream    : Ada.Streams.Stream_IO.Stream_Access;
      The_Filename   : constant String := Report.Legal_File_Name;
          
   begin

      -- If an implementation does not support Stream_IO in a particular
      -- environment, the exception Use_Error or Name_Error will be raised on 
      -- calls to various Stream_IO operations.  This block statement 
      -- encloses a call to Create, which should produce an exception in a 
      -- non-supportive environment.  These exceptions will be handled to 
      -- produce a Not_Applicable result.

      Ada.Streams.Stream_IO.Create (Data_File, 
                                    Ada.Streams.Stream_IO.Out_File,
                                    The_Filename);

      Operational_Test_Block:
      declare

         type Origin_Type is (Foreign, Domestic);
         subtype String_Data_Type is String(1..8);

         type Product_Type is
            record
               Item        : String_Data_Type;               
               ID          : Natural range 1..100;
               Manufacture : Origin_Type := Domestic;
               Distributor : String_Data_Type;
               Importer    : String_Data_Type;
            end record;

         type Sales_Record_Type is
            record                                      
               Name              : String_Data_Type;
               Sale_Item         : Boolean := False;
               Buyer             : Origin_Type;
               Quantity_Discount : Boolean;
               Cash_Discount     : Boolean;
            end record;


         -- Mode conformant, user defined subprograms that will override 
         -- the type-related attributes.
         -- In this test, the user defines these subprograms to add/subtract
         -- specific values from global variables.

         procedure Product_Read
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : out Product_Type );

         procedure Product_Write
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : Product_Type );

         procedure Sales_Read
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : out Sales_Record_Type );

         procedure Sales_Write
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : Sales_Record_Type );

         -- Attribute definition clauses.

         for Product_Type'Read  use Product_Read;
         for Product_Type'Write use Product_Write;

         for Sales_Record_Type'Read  use Sales_Read;
         for Sales_Record_Type'Write use Sales_Write;


         -- Object Declarations

         Product_01 : Product_Type := 
           ("Product1", 1, Domestic, "Distrib1", "Import 1");
         Product_02 : Product_Type := 
           ("Product2", 2, Foreign,  "Distrib2", "Import 2");

         Sale_Rec_01 : Sales_Record_Type := 
           ("Buyer 01", False, Domestic, True, True);
         Sale_Rec_02 : Sales_Record_Type := 
           ("Buyer 02", True,  Domestic, True, False);
         Sale_Rec_03 : Sales_Record_Type := (Name              => "Buyer 03", 
                                             Sale_Item         => True, 
                                             Buyer             => Foreign,  
                                             Quantity_Discount => False, 
                                             Cash_Discount     => True);
         Sale_Rec_04 : Sales_Record_Type := 
           ("Buyer 04", True,  Foreign,  False, False);
         Sale_Rec_05 : Sales_Record_Type := 
           ("Buyer 05", False, Foreign,  False, False);

         TC_Read_Total  : Integer := 100;
         TC_Write_Total : Integer :=   0;


         -- Subprogram bodies.
         -- These subprograms are designed to override the default attributes
         -- 'Read and 'Write for the specified types.  Each adds/subtracts
         -- a quantity to/from a program control variable, indicating its
         -- activity.   In addition, each component of the record is
         -- individually read from or written to the stream, using the 
         -- appropriate 'Read or 'Write attribute for the component type.
         -- The string components are moved to/from the stream using the
         -- 'Input and 'Output attributes for the string subtype, so that
         -- the bounds of the strings are also written/read.

         procedure Product_Read
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : out Product_Type ) is
         begin
            TC_Read_Total := TC_Read_Total - 10;

            The_Item.Item := String_Data_Type'Input(Data_Stream); -- Field 1.
            Natural'Read(Data_Stream, The_Item.ID);               -- Field 2.
            Origin_Type'Read(Data_Stream,                         -- Field 3.
                             The_Item.Manufacture);        
            The_Item.Distributor :=                               -- Field 4.
              String_Data_Type'Input(Data_Stream);
            The_Item.Importer    :=                               -- Field 5.  
              String_Data_Type'Input(Data_Stream);
         end Product_Read;


         procedure Product_Write
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : Product_Type ) is
         begin
            TC_Write_Total := TC_Write_Total + 5;

            String_Data_Type'Output(Data_Stream, The_Item.Item);  -- Field 1.
            Natural'Write(Data_Stream, The_Item.ID);              -- Field 2.
            Origin_Type'Write(Data_Stream,                        -- Field 3.
                             The_Item.Manufacture);        
            String_Data_Type'Output(Data_Stream,                  -- Field 4.
                                    The_Item.Distributor);
            String_Data_Type'Output(Data_Stream,                  -- Field 5.
                                    The_Item.Importer);
         end Product_Write;


         procedure Sales_Read
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : out Sales_Record_Type ) is
         begin
            TC_Read_Total := TC_Read_Total - 20;

            The_Item.Name := String_Data_Type'Input(Data_Stream);  -- Field 1.
            Boolean'Read(Data_Stream, The_Item.Sale_Item);         -- Field 2.
            Origin_Type'Read(Data_Stream, The_Item.Buyer);         -- Field 3.
            Boolean'Read(Data_Stream, The_Item.Quantity_Discount); -- Field 4.
            Boolean'Read(Data_Stream, The_Item.Cash_Discount);     -- Field 5.
         end Sales_Read;


         procedure Sales_Write
           ( Stream   : access Ada.Streams.Root_Stream_Type'Class;
             The_Item : Sales_Record_Type ) is
         begin
            TC_Write_Total := TC_Write_Total + 10;

            String_Data_Type'Output(Data_Stream, The_Item.Name);    -- Field 1.
            Boolean'Write(Data_Stream, The_Item.Sale_Item);         -- Field 2.
            Origin_Type'Write(Data_Stream, The_Item.Buyer);         -- Field 3.
            Boolean'Write(Data_Stream, The_Item.Quantity_Discount); -- Field 4.
            Boolean'Write(Data_Stream, The_Item.Cash_Discount);     -- Field 5.
         end Sales_Write;



      begin

         Data_Stream := Ada.Streams.Stream_IO.Stream (Data_File);

         -- Write product and sales data to the stream.

         Product_Type'Write      (Data_Stream, Product_01);
         Sales_Record_Type'Write (Data_Stream, Sale_Rec_01);
         Sales_Record_Type'Write (Data_Stream, Sale_Rec_02);

         Product_Type'Write      (Data_Stream, Product_02);
         Sales_Record_Type'Write (Data_Stream, Sale_Rec_03);
         Sales_Record_Type'Write (Data_Stream, Sale_Rec_04);
         Sales_Record_Type'Write (Data_Stream, Sale_Rec_05);

         -- Read data from the stream, and verify the use of the user specified
         -- attributes.

         Verify_Data_Block:
         declare

            TC_Product1,
            TC_Product2 : Product_Type;

            TC_Sale1, 
            TC_Sale2,
            TC_Sale3,
            TC_Sale4,
            TC_Sale5    : Sales_Record_Type;

         begin

            -- Reset the mode of the stream file so that Read/Input
            -- operations may be performed.

            Ada.Streams.Stream_IO.Reset (Data_File, 
                                         Ada.Streams.Stream_IO.In_File);

            -- Data is read/reconstructed from the stream, in the order that
            -- the data was placed into the stream.

            Product_Type'Read      (Data_Stream, TC_Product1);
            Sales_Record_Type'Read (Data_Stream, TC_Sale1);
            Sales_Record_Type'Read (Data_Stream, TC_Sale2);

            Product_Type'Read      (Data_Stream, TC_Product2);
            Sales_Record_Type'Read (Data_Stream, TC_Sale3);
            Sales_Record_Type'Read (Data_Stream, TC_Sale4);
            Sales_Record_Type'Read (Data_Stream, TC_Sale5);

            -- Verify product data was correctly written to/read from stream.

            if TC_Product1 /= Product_01 then
               Report.Failed ("Data verification error, Product 1");
            end if;
            if TC_Product2 /= Product_02 then
               Report.Failed ("Data verification error, Product 2");
            end if;

            if TC_Sale1 /= Sale_Rec_01 then
               Report.Failed ("Data verification error, Sale_Rec_01");
            end if;
            if TC_Sale2 /= Sale_Rec_02 then
               Report.Failed ("Data verification error, Sale_Rec_02");
            end if;
            if TC_Sale3 /= Sale_Rec_03 then
               Report.Failed ("Data verification error, Sale_Rec_03");
            end if;
            if TC_Sale4 /= Sale_Rec_04 then
               Report.Failed ("Data verification error, Sale_Rec_04");
            end if;
            if TC_Sale5 /= Sale_Rec_05 then
               Report.Failed ("Data verification error, Sale_Rec_05");
            end if;

            -- Verify that the user defined subprograms were used to
            -- override the default 'Read and 'Write attributes.
            -- There were two "product" reads and two writes; there 
            -- were five "sale record" reads and five writes.
            
            if (TC_Read_Total /= -20) or (TC_Write_Total /= 60) then
               Report.Failed ("Incorrect use of user defined attributes");
            end if;

         end Verify_Data_Block;

      exception

         when others => 
            Report.Failed ("Exception raised in Operational Test Block");
      
      end Operational_Test_Block;

      if Ada.Streams.Stream_IO.Is_Open (Data_File) then
         Ada.Streams.Stream_IO.Delete (Data_File);
      else
         Ada.Streams.Stream_IO.Open (Data_File,
                                     Ada.Streams.Stream_IO.Out_File, 
                                     The_Filename);
         Ada.Streams.Stream_IO.Delete (Data_File);
      end if;


   exception

      -- Since Use_Error or Name_Error can be raised if, for the specified
      -- mode, the environment does not support Stream_IO operations,
      -- the following handlers are included:

      when Ada.Streams.Stream_IO.Name_Error =>
         Report.Not_Applicable ("Name_Error raised on Stream IO Create");

      when Ada.Streams.Stream_IO.Use_Error  =>
         Report.Not_Applicable ("Use_Error raised on Stream IO Create");

      when others                           =>
         Report.Failed ("Unexpected exception raised");

   end Test_for_Stream_IO_Support;

   Report.Result;

end CXACA02;
