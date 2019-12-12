package Limited_Aggr is
   type R is limited record
      F1, F2 : Integer;
   end record;
   X : array (1..2) of integer;
   Y : R := (-111, -222);
   for Y'Address use X'Address;

   procedure Dummy;
end Limited_Aggr;
