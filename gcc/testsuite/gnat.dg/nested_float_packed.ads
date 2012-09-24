package Nested_Float_Packed is

   type Float_Type is record
      Value : Float;
      Valid : Boolean;
   end record;

   type Data_Type is record
      Data : Float_Type;
   end record;

   Default_Data : constant Data_Type :=
     (Data => (Value => 1.0, Valid => False));

   type Range_Type is (RV1, RV2, RV3);
   for Range_Type use (1, 2, 3);

   Data_Block : array (Range_Type)
     of Data_Type := (others => Default_Data);
end;
