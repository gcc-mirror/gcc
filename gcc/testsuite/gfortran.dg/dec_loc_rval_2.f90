! { dg-do compile }
! { dg-options "-std=gnu" }
!
! Test warnings for usage of %loc as an rvalue without -std=legacy.
!
program main
implicit none

integer, volatile :: i, j, k

i =  loc(j)
k = %loc(j) ! { dg-warning "Legacy Extension:" }

end
