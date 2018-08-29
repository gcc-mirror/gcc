package Rep_Clause6 is

   type B1_Type is range 0 .. 2**1 - 1;
   for B1_Type'Size use 1;

   type U10_Type is range 0 .. 2**10 - 1;
   for U10_Type'Size use 10;

   type B5_Type is range 0 .. 2**5 - 1;
   for B5_Type'Size use 5;

   type B11_Type is range 0 .. 2**11 - 1;
   for B11_Type'Size use 11;

   type Rec1 is record
      B1  : B1_Type;
      U10 : U10_Type;
      B5  : B5_Type;
   end record;

   for Rec1 use record
      B1  at 0 range 0  ..  0;
      U10 at 0 range 1  .. 10;
      B5  at 0 range 11 .. 15;
   end record;
   for Rec1'Size use 16;

   type Arr is array (1 .. 5) of Rec1;
   for Arr'Size use 80;

   subtype Header_Type is String (1 .. 16);

   type Rec2 is record
      Header          : Header_Type;
      Spare_5         : B5_Type;
      Deleted_Reports : Arr;
      Block_End       : B11_Type;
   end record;

   for Rec2 use record
      Header          at 0  range 0  .. 127;
      Spare_5         at 16 range 0  ..   4;
      Deleted_Reports at 16 range 5  ..  84;
      Block_End       at 24 range 21 ..  31;
   end record;
   for Rec2'Size use 224;

   type Enum is (A_Msg, B_Msg, C_Msg, D_Msg);

   type Rec3 (Msg_Type : Enum := Enum'First) is record
      case Msg_Type is
         when A_Msg => A_M : Arr;
         when B_Msg => B_M : Arr;
         when C_Msg => C_M : Rec2;
         when others => null;
      end case;
   end record;

   procedure Dummy;

end Rep_Clause6;
