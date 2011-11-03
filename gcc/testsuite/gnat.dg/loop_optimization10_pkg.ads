package Loop_Optimization10_Pkg is

   pragma Pure (Loop_Optimization10_Pkg);

   type Limit_Type is record
      Low  : Float;
      High : Float;
   end record;

   function F (Low, High : in Float) return Limit_Type;

end Loop_Optimization10_Pkg;
