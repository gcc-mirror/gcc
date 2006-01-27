! { dg-do compile }
! Tests the fix for PR25951, a regression caused by the assumed
! size patch.
! Test case provided by Mark Hesselink  <mhesseli@caltech.edu>
PROGRAM loc_1
  integer i(10)
  call f (i)
CONTAINS
   SUBROUTINE f (x)
      INTEGER, DIMENSION(*)   :: x
      INTEGER                 :: address
! The next line would cause:
! Error: The upper bound in the last dimension must appear in the
! reference to the assumed size array 'x' at (1)
      address=LOC(x)
   END SUBROUTINE f
END PROGRAM loc_1