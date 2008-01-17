-- { dg-do compile }

procedure Discr5 is

   type Enum is (Ten, Twenty);
   for Enum use (10, 20);
   type Arr is array (Enum range <>) of Integer;
   type Rec (Discr: Enum := Ten) is record
      case Discr is
         when others =>
            A: Arr (Ten .. Discr);
      end case;
   end record;

begin
   null;
end;
