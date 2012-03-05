! { dg-do compile}
!
! PR fortran/41587
! This program was leading to an ICE related to class allocatable arrays
!
! Contributed by Dominique D'Humieres <dominiq@lps.ens.fr>

type t0
  integer :: j = 42
end type t0
type t
  integer :: i
  class(t0), allocatable :: foo(:)
end type t
type(t) :: k
allocate(t0 :: k%foo(3))
print *, k%foo%j
end
