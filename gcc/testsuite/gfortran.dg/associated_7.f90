! { dg-do compile }
!
! PR 55692: ICE on incorrect use of ASSOCIATED function
!
! Contributed by Gilbert Scott <gilbert.scott@easynet.co.uk>

INTEGER, POINTER :: P1, P2
PRINT *, ASSOCIATED([P1,P2])   ! { dg-error "must be a POINTER" }
END
