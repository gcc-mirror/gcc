! { dg-do compile }
!
! PR 87172: [9 Regression] Spurious "Derived type 'c_funptr' at (1) has not been declared" error after r263782
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

module m1
   use iso_c_binding, only: c_funptr
end module

module m2
  use m1
  use iso_c_binding
end module
