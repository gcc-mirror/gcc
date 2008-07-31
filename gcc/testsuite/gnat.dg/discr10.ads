package Discr10 is

   subtype Index is Natural range 0 .. 150;

   type List is array (Index range <>) of Integer;

   type R (D1 : Boolean := True; D2 : Boolean := False; D3 : Index := 0) is
   record
      case D2 is
         when True =>
            L : List (1 .. D3);
            case D1 is
               when True => I : Integer;
               when False => null;
            end case;
         when False =>
            null;
      end case;
   end record;

   function Get (X : R) return R;

end Discr10;
