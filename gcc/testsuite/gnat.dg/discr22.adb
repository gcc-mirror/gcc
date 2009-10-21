-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Discr22 is

   subtype Precision is Integer range 1 .. 5;

   type Rec(D1 : Precision; D2 : Integer) is record
      case D1 is
         when 1 => I : Integer;
         when others => null;
      end case;
   end record;
   for Rec use record
      D1 at 0 range 0 .. 7;
   end record;

   P : Precision;
   X : Rec(P, 0);

begin
   null;
end;
