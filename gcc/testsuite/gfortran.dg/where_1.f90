! { dg-do run }
! Tests the fix for PR35759 and PR35756 in which the dependencies
! led to an incorrect use of the "simple where", gfc_trans_where_3.
!
! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>
!
  logical :: la(6) = (/(2*(i/2) /= i, i = 1, 6)/), lb(6)
  CALL PR35759
  CALL PR35756
!
! The first version of the fix caused this to regress as pointed
! out by Dominique d'Humieres
!
  lb = la
  where(la)
    la = .false.
  elsewhere
    la = .true.
  end where
  if (any(la .eqv. lb)) call abort()
CONTAINS
  subroutine PR35759
    integer UDA1L(6)
    integer ::  UDA1R(6), expected(6) = (/2,0,5,0,3,0/)
    LOGICAL LDA(5)
    UDA1L(1:6) = 0
    uda1r = (/1,2,3,4,5,6/)
    lda = (/ (i/2*2 .ne. I, i=1,5) /)
    WHERE (LDA)
      UDA1L(1:5) = UDA1R(2:6)
    ELSEWHERE
      UDA1L(2:6) = UDA1R(6:2:-1)
    ENDWHERE
    if (any (expected /= uda1l)) call abort
  END subroutine

  SUBROUTINE PR35756
    INTEGER  ILA(10), CLA(10)
    LOGICAL  LDA(10)
    ILA = (/ (I, i=1,10) /)
    LDA = (/ (i/2*2 .ne. I, i=1,10) /)
    WHERE(LDA)
      CLA = 10
    ELSEWHERE
      CLA = 2
    ENDWHERE
    WHERE(LDA)
      ILA = R_MY_MAX_I(ILA)
    ELSEWHERE
      ILA = R_MY_MIN_I(ILA)
    ENDWHERE
    IF (any (CLA /= ILA)) call abort
  end subroutine

  INTEGER FUNCTION R_MY_MAX_I(A)
    INTEGER  ::  A(:)
    R_MY_MAX_I = MAXVAL(A)
  END FUNCTION R_MY_MAX_I

  INTEGER FUNCTION R_MY_MIN_I(A)
    INTEGER  ::  A(:)
    R_MY_MIN_I = MINVAL(A)
  END FUNCTION R_MY_MIN_I
END
