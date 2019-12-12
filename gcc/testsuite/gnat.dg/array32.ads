package Array32 is

   type Rec is record
      I : Integer;
   end record;

   type Arr is array (Positive range <>) of Rec;

   procedure Init (A : out Arr);

end Array32;
