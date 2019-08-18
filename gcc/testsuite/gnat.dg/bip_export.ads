package Bip_Export is
   type T is limited null record;
   function F return T;
   pragma Export (C, F);
   function G return T;
end Bip_Export;
