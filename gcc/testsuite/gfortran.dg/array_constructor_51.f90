! { dg-do compile }
! { dg-additional-options "-ffrontend-optimize -fdump-tree-original" }
! PR 82567 - long compile times caused by large constant constructors
! multiplied by variables

  SUBROUTINE sub()
  IMPLICIT NONE
  
  INTEGER, PARAMETER :: n = 1000
  REAL, ALLOCATABLE :: x(:)
  REAL :: xc, h
  INTEGER :: i
 
  ALLOCATE( x(n) )
  xc = 100.
  h = xc/n
  x = h*[(i,i=1,n)]
  
end
! { dg-final { scan-tree-dump-times "__var" 0 "original" } }
