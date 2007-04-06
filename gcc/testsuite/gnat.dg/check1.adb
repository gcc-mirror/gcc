-- { dg-do compile }

package body Check1 is
   function FD (X : access R) return P2 is
   begin
      return P2 (X.Disc);
   end FD;
end Check1;
