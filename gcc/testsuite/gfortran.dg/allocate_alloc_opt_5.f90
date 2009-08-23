! { dg-do compile }
! { dg-options "-std=f95" }
program a

  implicit none

  integer n
  character(len=70) str
  integer, allocatable :: i(:)

  n = 42
  allocate(i(4), source=n) ! { dg-error "Fortran 2003: SOURCE tag" }
  allocate(i(4), stat=n, errmsg=str) ! { dg-error "Fortran 2003: ERRMSG tag" }

end program a
