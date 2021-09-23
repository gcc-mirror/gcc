! { dg-do run }
!
! Test the fix for PR94331
!
! Contributed by Robin Hogan <r.j.hogan@reading.ac.uk>
!

program test 

  use, intrinsic :: iso_c_binding, only: &
    c_int, c_float

  implicit none

  integer                       :: i
  integer,            parameter :: n = 11
  real(kind=c_float), parameter :: u(*) = [(real(i, kind=c_float), i=1,n)]
  
  real(kind=c_float), allocatable :: A(:)
  real(kind=c_float)              :: E(n)
  integer(kind=c_int)             :: l1, l2, l3

  allocate(A, source=u)
  l1 = lbound(A, 1)
  call routine_bindc(A, l2) ! in gcc-9.2.1 this changes lbound of A...
  l3 = lbound(A, 1)
  if (l1 /= 1)                        stop 1
  if (l1 /= l2)                       stop 2
  if (l1 /= l3)                       stop 3
  if (any(abs(A(1:n)-u)>0.0_c_float)) stop 4
  deallocate(A)
  !
  E = u
  l1 = lbound(E, 1)
  call routine_bindc(E, l2) ! ...but does not change lbound of E
  l3 = lbound(E, 1)
  if (l1 /= 1)                        stop 5
  if (l1 /= l2)                       stop 6
  if (l1 /= l3)                       stop 7
  if (any(abs(E(1:n)-u)>0.0_c_float)) stop 8

contains

  subroutine routine_bindc(v, l) bind(c)
    real(kind=c_float),  intent(inout) :: v(:)
    integer(kind=c_int), intent(out)   :: l
    
    l = lbound(v, 1)
    if (any(abs(v(1:n)-u)>0.0_c_float)) stop 9
  end subroutine routine_bindc
  
end program test
