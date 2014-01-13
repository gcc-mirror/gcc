package Loop_Optimization17_Pkg is

   type vector is array (1..3) of Long_Float;

   type Rec is
      record
         I : Integer;
         V1, V2 : Vector;
         S : Long_Float;
      end record;

   for  Rec  use
      record
         I  at   0 range  0 .. 31;
         V1 at   4 range  0 .. 191;
         V2 at  28 range  0 .. 191;
         S  at  52 range  0 .. 63;
      end  record;
   for Rec'Alignment use 4;
   for Rec'Size use 480;

   type Index_T is range 1 .. 5;
   type Arr is array (Index_T) of Rec;

   Object : Arr;

   function F (V : Vector) return Vector;

end Loop_Optimization17_Pkg;
