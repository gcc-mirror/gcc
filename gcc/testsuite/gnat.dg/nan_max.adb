-- { dg-do run }

procedure Nan_Max is

   function NaN return Long_Float is
      Zero : Long_Float := 0.0;
   begin
      return Zero / Zero;
   end;

   Z : Long_Float := 1.0;
   N : Long_Float := NaN;

begin
   if Long_Float'Max (N, Z) /= Z then
      raise Program_Error;
   end if;

   if Long_Float'Max (Z, N) /= Z then
      raise Program_Error;
   end if;

   if Long_Float'Max (NaN, Z) /= Z then
      raise Program_Error;
   end if;

   if Long_Float'Max (Z, NaN) /= Z then
      raise Program_Error;
   end if;
end;
