-- { dg-do compile }

package Discr8 is

  type T1 (N : Natural) is null record;

  type T2 (N : Natural) is record
    C1 : string (1 .. T2.n); -- { dg-error "alone as a direct name" }
    C2 : string (1 .. n);
    C3 : T1 (T2.n); -- { dg-error "alone as a direct name" }
    C4 : T1 (n);
  end record;

end Discr8;
