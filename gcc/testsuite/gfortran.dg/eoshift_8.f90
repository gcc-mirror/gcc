! { dg-do compile }
! PR 83664 - invalid code that used to be accepted.
program main
  implicit none
  integer :: n
  integer :: i,n1, n2, n3
  character(len=3), parameter :: e(2,3,4) = reshape([(repeat(achar(i),3),i=iachar('a'),iachar('a')+2*3*4-1)], &
       shape(e))
  character(len=3), parameter :: bnd2(3,5) = reshape([(repeat(achar(i),3),i=iachar('A'),iachar('A')+3*5-1)], &
       shape(bnd2))
  character(len=3) :: f2(2,3,4) 

  n = -1
  f2 = eoshift(e,shift=n,boundary=bnd2) ! { dg-error "has invalid shape" }
  f2 = eoshift(e,shift=1,boundary="x") ! { dg-error "must be of same type and kind" }

  print '(*(1H",A,1H",:","))',f2
end program main
