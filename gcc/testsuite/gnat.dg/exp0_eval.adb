-- { dg-do run }
with Interfaces; use Interfaces;
procedure Exp0_Eval is

   F_Count : Natural := 0;

   function F return Integer is
   begin
      F_Count := F_Count + 1;
      return 1;
   end F;

   function F return Unsigned_32 is
   begin
      F_Count := F_Count + 1;
      return 1;
   end F;

   R : constant Integer :=
     F ** 0 +
     F * 0 +
     0 * F +
     Integer (Unsigned_32'(F) mod 1) +
     Integer (Unsigned_32'(F) rem 1);
   pragma Warnings (Off, R);
begin
   if F_Count /= 5 then
      raise Program_Error
        with "incorrect numbers of calls to F:" & F_Count'Img;
   end if;
end Exp0_Eval;
