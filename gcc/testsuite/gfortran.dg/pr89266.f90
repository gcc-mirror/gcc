! { dg-do run }
!
! PR fortran/89266 - ICE with TRANSFER of len=0 character array constructor

program test
  implicit none
  character(*), parameter :: n = ''
  character(*), parameter :: o = transfer ([''], n)
  character(*), parameter :: p = transfer ( n , n)
  character(*), parameter :: q = transfer ([n], n)
  character(6), save      :: r = transfer ([''], n)
  character(6), save      :: s = transfer ( n , n)
  character(6), save      :: t = transfer ([n], n)
  integer,      parameter :: a(0) = 0
  integer,      parameter :: b(0) = transfer (a, a)
  integer,      save      :: c(0) = transfer (a, a)
  if (len (o) /= 0) stop 1
  if (len (p) /= 0) stop 2
  if (len (q) /= 0) stop 3
  if (r /= "") stop 4
  if (s /= "") stop 5
  if (t /= "") stop 6
  if (size (b) /= 0 .or. any (b /= 0)) stop 7
  if (size (c) /= 0 .or. any (c /= 0)) stop 8
end program test
