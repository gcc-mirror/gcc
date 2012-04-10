-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Aggr17 is

   type Enum is (A, B);

   type Rec (D : Enum := Enum'First) is record
      case D is
         when A => X : Integer;
         when B => null;
      end case;
   end record;
   for Rec'Size use 128;
   pragma Volatile (Rec);

   type Config_T (D : Enum := Enum'First) is record
      N : Natural;
      R : Rec (D);
   end record;

   C : constant Config_T := (D => A, N => 1, R => (D => A, X => 0));

   type Arr is array (Natural range 1 .. C.N) of Boolean;

begin
   null;
end;
