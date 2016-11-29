! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Test errors for usage of %loc as an rvalue with a real standard.
!
program main
implicit none

integer, volatile :: i, j, k

k = %loc(j) ! { dg-error "Legacy Extension:" }

end
