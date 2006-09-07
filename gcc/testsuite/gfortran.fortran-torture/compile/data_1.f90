! this tests the fix for PR 13826
TYPE a
   REAL x
END TYPE
TYPE(a) :: y
DATA y /a(1.)/ ! used to give an error about non-PARAMETER
END
! this tests the fix for PR 13940
SUBROUTINE a
DATA i /z'f95f95'/
END
