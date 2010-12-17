! { dg-do compile }
!
! PR 45577: [4.6 Regression] Bogus(?) "... type incompatible with source-expr ..." error
!
! Contributed by Dominique d'Humieres <dominiq@lps.ens.fr>

program main

type b_obj
  integer,allocatable :: c(:)
  real :: r = 5.
end type b_obj

type (b_obj),allocatable :: b(:)
integer,allocatable :: c(:)

allocate(b(3),c(3))

end program main 
