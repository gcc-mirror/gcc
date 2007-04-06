package Check1 is
   type Arr is array (Integer range <>) of Integer;
   type P2 is access all Arr;
   type R (Disc : access Arr) is limited null record;
   function FD (X : access R) return P2;
end Check1;
