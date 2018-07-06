! { dg-do run }
! { dg-options "-std=legacy" }
!
! Test the usage of %loc as an rvalue.
!
program main
implicit none

integer :: i, j, k

i =  loc(j)
k = %loc(j)

if (i .ne. k) then
  print *, "bad %loc value"
  STOP 1
endif

end
