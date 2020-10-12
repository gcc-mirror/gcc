! { dg-do run }
! PR fortran/94672
!
! Contributed by Tomáš Trnka
!
module m
  implicit none (type,external)
  type t
    integer :: i = 5
  end type t
contains
subroutine bar(x, y, z, n)
  integer, value :: n
  type(t), intent(out), optional :: x(:), y(n), z(:)
  allocatable :: z
end subroutine bar

subroutine foo (n, nFound, sVal)
   integer,                   value  :: n
   integer,                   intent(out)  :: nFound
   character(*),    optional, intent(out) :: sVal(n)

   nFound = 0

   if (present(sVal)) then
      nFound = nFound + 1
   end if
end subroutine
end

use m
implicit none (type,external)
type(t) :: a(7), b(7), c(:)
allocatable :: c
integer :: nn, nf
character(len=4) :: str

allocate(c(7))
call bar(a,b,c,7)
if (any(a(:)%i /= 5)) stop 1
if (any(b(:)%i /= 5)) stop 2
if (allocated(c)) stop 3

call foo(7, nf, str)
if (nf /= 1) stop 4
call foo(7, nf)
if (nf /= 0) stop 5
end
