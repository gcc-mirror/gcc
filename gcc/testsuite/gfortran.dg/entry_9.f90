! { dg-do run }
! Check whether RESULT of ENTRY defaults to entry-name.
! PR fortran/30873
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M1
  CONTAINS
    FUNCTION F2(K)
      INTEGER :: F2,K
      F2=E1(K)
    END FUNCTION F2

    RECURSIVE FUNCTION F1(I)
      INTEGER :: F1,I,E1
      F1=F2(I)
      RETURN
     ENTRY E1(I)
      E1=-I
      RETURN
    END FUNCTION F1
END  MODULE M1

program main
  use m1
  if (E1(5) /= -5) STOP 1
  if (F2(4) /= -4) STOP 2
  if (F1(1) /= -1) STOP 3
end program main
