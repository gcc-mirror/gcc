--  { dg-do compile }

package body Pure_Function1 is
   function F return Integer is (0);
   pragma Pure_Function (F);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
   pragma Pure_Function (F);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
   pragma Pure_Function (F);  --  { dg-error "pragma \"Pure_Function\" argument must be in same declarative part" }
end;
