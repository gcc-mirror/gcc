package Corr_Discr is

   type Base (T1 : Boolean := True; T2 : Boolean := False)
     is null record;
   for Base use record
      T1 at 0 range 0 .. 0;
      T2 at 0 range 1 .. 1;
   end record;

   type Deriv (D : Boolean := False) is new Base (T1 => True, T2 => D);

end Corr_Discr;

