! { dg-do compile }
! { dg-options "-std=f2003" }
program a

  implicit none

  integer n, m(3,3)
  integer(kind=8) k
  integer, allocatable :: i(:), j(:)
  real, allocatable :: x(:)

  n = 42
  m = n
  k = 1_8

  allocate(i(4), source=42, source=n) ! { dg-error "Redundant SOURCE tag found" }

  allocate(integer(4) :: i(4), source=n) ! { dg-error "conflicts with the typespec" }

  allocate(i(4), j(n), source=n) ! { dg-error "Fortran 2008: SOURCE tag at .1. with more than a single allocate object" }

  allocate(x(4), source=n) ! { dg-error "type incompatible with" }

  allocate(i(4), source=m) ! { dg-error "must be scalar or have the same rank" }

  allocate(i(4), source=k) ! { dg-error "shall have the same kind type" }

end program a
