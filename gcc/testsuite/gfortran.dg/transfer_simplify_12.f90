! { dg-do run }
! { dg-options "-O -std=legacy" }
!
! Test fixes for some findings while resolving PR fortran/89077

program test
  implicit none
  integer :: i
  character(*)  ,parameter :: s =  'abcdef'   ! Length will be 6
  character(*)  ,parameter :: h = 6Habcdef    ! Length will be 8 (Hollerith!)
  character(10) ,parameter :: k = 6Habcdef
  character(10) ,parameter :: t = transfer (s, s)
  character(10) ,save      :: u = transfer (s, s)
  character(10) ,parameter :: v = transfer (h, h)
  character(10) ,save      :: w = transfer (h, h)
  character(10) ,parameter :: x = transfer ([(s(i:i),i=len(s),1,-1)], s)
  character(10) ,save      :: y = transfer ([(s(i:i),i=len(s),1,-1)], s)
  if (len (h) /= 8) stop 1
  if (h /= s) stop 2
  if (k /= s) stop 3
  if (t /= s) stop 4
  if (u /= s) stop 5
  if (v /= s) stop 6
  if (w /= s) stop 7
  if (x /= "fedcba") stop 8
  if (y /= x) stop 9
end program test
