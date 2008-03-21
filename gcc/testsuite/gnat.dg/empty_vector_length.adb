--  { dg-do run }
--  { dg-options "-gnatp" }

procedure Empty_Vector_Length is

   type Vector is array (Integer range <>) of Integer;

   function Empty_Vector return Vector is
   begin
      return (2 .. Integer'First => 0);
   end;

   My_Vector : Vector := Empty_Vector;
   My_Length : Integer := My_Vector'Length;
begin
   if My_Length /= 0 then
      raise Program_Error;
   end if;
end;
