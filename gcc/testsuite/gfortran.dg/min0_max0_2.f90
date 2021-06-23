! { dg-do compile }
! { dg-options "-fdefault-integer-8 -std=gnu" }
! PR fortran/101123

SUBROUTINE TEST
  IMPLICIT INTEGER*4 (I-N)
  MAXMN=MAX0(M,N)
  MINMN=MIN0(M,0_4)
  MAXRS=MAX1(R,S)
END SUBROUTINE TEST
