--  { dg-do run }

with Interfaces;

procedure Pack24 is

   type Enum_1 is (Lit_1);
   for Enum_1'SIZE use 16;

   type Rec1(D1 : Enum_1 := Lit_1) is
      record
         case D1 is
         when Lit_1 =>
            F1 : Interfaces.Unsigned_16;
         when others =>
            Null;
         end case;
      end record;
   pragma Pack(Rec1);

   type Rec2 is
      record
         F1 : Interfaces.Unsigned_16;
         F2 : Rec1;
   end record;
   pragma Pack(Rec2);

   type Rec3 is record
      F1 : Interfaces.Unsigned_8;
      F2 : Rec2;
   end record;
   pragma Pack(Rec3);

begin
  if Rec3'Size /= 56 then
    raise Program_Error;
  end if;
end;
