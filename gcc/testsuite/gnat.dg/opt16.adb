-- { dg-do compile }
-- { dg-options "-O2 -gnatws" }

procedure Opt16 is

   generic
      type T (<>) is private;
      V, V1 : T;
      with function F1 (X : T) return T;
   package GP is
      R : Boolean := F1 (V) = V1;
   end GP;

   type AB is array (Boolean range <>) of Boolean;

begin
   for I1 in Boolean loop
      for I2 in Boolean loop
         declare
            B1 : Boolean := I1;
            B2 : Boolean := I2;
            AB1 : AB (Boolean) := (I1, I2);
            T : AB (B1 .. B2) := (B1 .. B2 => True);
            F : AB (B1 .. B2) := (B1 .. B2 => False);

            package P is new GP (AB, AB1, NOT AB1, "NOT");

         begin
            null;
         end;
      end loop;
   end loop;
end;
