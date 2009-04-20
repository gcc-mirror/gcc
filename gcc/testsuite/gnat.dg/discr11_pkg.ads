package Discr11_Pkg is
   type DT_1 (<>) is tagged private;
   function Create return DT_1;
private
   type DT_1 (Size : Positive) is tagged record
      Data : String (1 .. Size);
   end record;
end Discr11_Pkg;
