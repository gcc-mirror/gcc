--  { dg-do compile }

with Unchecked_Conversion;
procedure warn2 is
   type R1 is record X : Integer; end record;
   type R2 is record X, Y : Integer; end record;
   pragma Warnings
     (Off, "types for unchecked conversion have different sizes");
   function F is new Unchecked_Conversion (R1, R2);
   pragma Warnings
     (On, "types for unchecked conversion have different sizes");
begin
   null;
end warn2;
