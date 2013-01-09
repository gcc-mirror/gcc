-- { dg-do run }

procedure Alignment10 is

   type Short_T is mod 2 ** 16;
   for Short_T'Size use 16;
   for Short_T'Alignment use 1;

   subtype Short_Sub_T is Short_T range 1000 .. 1005;

   A : aliased Short_T := 1000;
   B : Short_Sub_T;
   for B'Address use A'Address;
   pragma Import (Ada, B);

begin
  if B /= 1000 then
    raise Program_Error;
  end if;
end;
