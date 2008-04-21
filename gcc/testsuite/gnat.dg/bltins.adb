-- { dg-do run }

procedure Bltins is

   function Sqrt (F : Float) return Float;
   pragma Import (Intrinsic, Sqrt, "__builtin_sqrtf");

   F : Float := 4.0;
   R : Float;
begin
   R := Sqrt (F);
end;
