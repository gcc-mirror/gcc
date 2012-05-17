! { dg-do compile }
! Tests the fix for PR30880, in which the variable d1
! in module m1 would cause an error in the main program
! because it has an initializer and is a dummy.  This
! came about because the function with multiple entries
! assigns the initializer earlier than for other cases.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk> 
!
MODULE M1
  TYPE T1
  INTEGER :: i=7
  END TYPE T1
CONTAINS
  FUNCTION F1(d1) RESULT(res)
    INTEGER :: res
    TYPE(T1), INTENT(OUT) :: d1
    TYPE(T1), INTENT(INOUT) :: d2
    res=d1%i
    d1%i=0
    RETURN
  ENTRY E1(d2) RESULT(res)
    res=d2%i
    d2%i=0
  END FUNCTION F1
END MODULE M1

  USE M1
  TYPE(T1) :: D1
  D1=T1(3)
  write(6,*) F1(D1)
  D1=T1(3)
  write(6,*) E1(D1)
END
