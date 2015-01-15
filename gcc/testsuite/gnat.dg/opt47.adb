-- { dg-do run { target i?86-*-* x86_64-*-* alpha*-*-* ia64-*-* } }
-- { dg-options "-O2" }

with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Interfaces;                use Interfaces;
with Ada.Unchecked_Conversion;

procedure Opt47 is

   subtype String4 is String (1 .. 4);
   function To_String4 is new Ada.Unchecked_Conversion (Unsigned_32, String4);
   type Arr is array (Integer range <>) of Unsigned_32;
   Leaf : Arr (1 .. 4) := (1349478766, 1948272498, 1702436946, 1702061409);
   Value : Unsigned_32;
   Result : String (1 .. 32);
   Last : Integer := 0;

begin
   for I in 1 .. 4 loop
      Value := Leaf (I);
      for J in reverse String4'Range loop
         if Is_Graphic (To_String4 (Value)(J)) then
            Last := Last + 1;
            Result (Last) := To_String4 (Value)(J);
         end if;
      end loop;
   end loop;
   if Result (1) /= 'P' then
      raise Program_Error;
   end if;
end;
