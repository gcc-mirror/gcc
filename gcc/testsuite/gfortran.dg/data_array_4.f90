! { dg-do compile }
! PR32928 DATA statement with array element as initializer is rejected
IMPLICIT NONE
INTEGER , PARAMETER :: NTAB = 3
REAL :: SR(NTAB) , SR3(NTAB)
DATA SR/NTAB*0.0/ , SR3/NTAB*0.0/
end
