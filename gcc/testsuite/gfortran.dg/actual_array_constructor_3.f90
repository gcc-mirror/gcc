! { dg-do run }
! Tests the fix for pr28914, in which array constructors using the loop
! variable within a do loop for the implied do loop of the constructor
! would result in a corrupted do loop counter.
!
! Based on the testscase by Ed Korkven <kornkven@arsc.edu>
!
program pr28914
  implicit none
  integer n, i
  parameter (n = 66000) ! Problem manifests for n > 65535
  double precision a(n), summation

  summation = 0.0
  do i = 1, 1
    a = (/ (i, i = 1, n) /) ! This is legal and was broken
    a = sqrt(a)
    summation = SUM(a)
  enddo
  summation = abs(summation - 11303932.9138271_8)
  
  if (summation.gt.0.00001)   STOP 1
end program pr28914


