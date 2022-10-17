-- { dg-do compile }

with System;

package Discr5 is

   X, Y : Boolean;

   type R (D : Boolean := False) is tagged limited record
      F : Integer;
      case D is
         when True =>
            F1, F2 : Integer;
         when False =>
            null;
      end case;
   end record;
   for R use record
      F1 at 100 range 0..31;
   end record;
   
   subtype Rt is R(True);
   subtype Rf is R(False);

   type R1 (D1 : Boolean) is new R (X) with record -- { dg-warning "\"X\" may be referenced before it has a value" }
      FF : Float;
      case D1 is
         when True =>
            F3, F4 : Float;
         when False =>
            null;
      end case;
   end record;
   for R1 use record
      F4 at 200 range 0..31;
   end record;

   subtype R1t is R1 (True);
   subtype R1f is R1 (False);

   type R2 (D2 : Boolean) is new R1 (Y) with record -- { dg-warning "\"Y\" may be referenced before it has a value" }
      FFF: System.Address;
      case D2 is
         when True =>
            F5, F6: System.Address;
         when False =>
            null;
      end case;
   end record;
   for R2 use record
      F6 at 300 range 0..63;
   end record;

   subtype R2t is R2 (True);
   subtype R2f is R2 (False);

end Discr5;

