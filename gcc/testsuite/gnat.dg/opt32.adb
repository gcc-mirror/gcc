-- { dg-do compile }
-- { dg-options "-O2" }

with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;

function Opt32 return Natural is

   package My_Vectors
      is new Vectors (Index_Type => Natural, Element_Type => Integer);
   use My_Vectors;

   V : Vector;

   function Sign_Changes return Natural is
      Cur      : Cursor := To_Cursor (V, 0);
      R        : Natural := 0;
      Negative : Boolean;
   begin
      Negative := Element (Cur) < 0;

      loop
         Cur := Next (Cur);
         exit when R > 100;

         if (Element (Cur) < 0) /= Negative then
            Negative := not Negative;
            R := R + 1;
         end if;
      end loop;

      return R;
   end;

begin
   return Sign_Changes;
end;
