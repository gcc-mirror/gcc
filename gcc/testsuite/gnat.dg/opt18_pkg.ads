package Opt18_Pkg is

   pragma Pure;

   type Limit_Type is record
      Low  : Float;
      High : Float;
   end record;

   function First_Order_Trig return Float;

  type Trig_Pair_Type is
   record
      Sin : Float;
      Cos : Float;
   end record;

   function Atan2 (Trig : in Trig_Pair_Type) return Float;

   function Unchecked_Trig_Pair (Sin, Cos : in Float) return Trig_Pair_Type;

   function Double_Trig (Trig : in Trig_Pair_Type) return Trig_Pair_Type;

   function Sqrt (X : Float) return Float;

end Opt18_Pkg;
