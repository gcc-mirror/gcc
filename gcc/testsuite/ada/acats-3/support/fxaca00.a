-- FXACA00.A
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
-- FOUNDATION DESCRIPTION:
--      This foundation consists of type definitions and object declarations 
--      used by tests of Stream_IO functionality.
--      Objects of both record types specified below (discriminated records
--      with defaults, and discriminated records w/o defaults that have the
--      discriminant included in a representation clause for the type) should 
--      have their discriminants included in the stream when using 'Write 
--      Likewise, discriminants should be extracted from the stream when
--      using 'Read.
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with ImpDef;

package FXACA00 is

   type Origin_Type is (Foreign, Domestic);

   for Origin_Type'Size use 1;   -- Forces objects of the type to be
                                 -- representable in 1 bit, used in rep clause
                                 -- below for Sales_Record_Type.

   type Product_Type (Manufacture : Origin_Type := Domestic) is
      record
         Item : String (1..8);               
         ID   : Natural range 1..100;
         case Manufacture is
            when Foreign  =>
               Importer : String (1..10);
            when Domestic =>
               Distributor : String (1..10);
         end case;
      end record;


   type Sales_Record_Type (Buyer : Origin_Type) is   -- No default provided
      record                                         -- for the discriminant.
         Name      : String (1..6);
         Sale_Item : Boolean := False;
         case Buyer is
            when Foreign  =>
               Quantity_Discount : Boolean;
            when Domestic =>
               Cash_Discount : Boolean;
         end case;
      end record;


   String_Bits : constant := ImpDef.Char_Bits * 6 - 1;

   -- This discriminated record type has a representation clause that 
   -- includes the discriminant of the object of this type.

   for Sales_Record_Type use                    
      record                                    
         Name              at 0 range 0..String_Bits;    
         Sale_Item         at ImpDef.Next_Storage_Slot range 0..0;     
         Buyer             at ImpDef.Next_Storage_Slot range 1..1;     
         Quantity_Discount at ImpDef.Next_Storage_Slot range 2..2;
         Cash_Discount     at ImpDef.Next_Storage_Slot range 3..3;
      end record;


   type Timespan_Type is (Week, Month, Year);

   type Sales_Statistics_Type is 
      array (Timespan_Type) of natural range 0 .. 500;


   -- Object Declarations


   Product_01 : Product_Type := (Domestic, "Product1", 1, "Distrib 01");
   Product_02 : Product_Type (Manufacture => Foreign) := (Foreign,
                                                          "Product2",
                                                          2,
                                                          "Importer02");
   Product_03 : Product_Type (Foreign) := (Manufacture => Foreign,
                                           Item        => "Product3",
                                           ID          => 3,
                                           Importer    => "Importer03");
   --

   Sale_Count_01 : Integer := 2;
   Sale_Count_02 : Integer := 0;
   Sale_Count_03 : Integer := 3;

   --

   Sale_Rec_01 : Sales_Record_Type (Domestic) := 
     (Domestic, "Buyer1", False, True);
   Sale_Rec_02 : Sales_Record_Type (Domestic) := 
     (Domestic, "Buyer2", True,  False);

   Sale_Rec_03 : Sales_Record_Type (Buyer => Foreign) := 
     (Buyer => Foreign, Name => "Buyer3", Sale_Item => True, 
      Quantity_Discount => True);

   Sale_Rec_04 : Sales_Record_Type (Foreign) := 
     (Foreign, "Buyer4", True, False);
   Sale_Rec_05 : Sales_Record_Type (Buyer => Foreign) := (Foreign,
                                                          "Buyer5",
                                                          False,
                                                          False);
   --

                                                       
   Product_01_Stats : Sales_Statistics_Type := (2,4,8);
   Product_02_Stats : Sales_Statistics_Type := (Week  =>  0,
                                                Month =>  5,
                                                Year  => 10);
   Product_03_Stats : Sales_Statistics_Type := (3, 6, others => 12);


end FXACA00;
