! { dg-do compile }
! Tests the fix for PR29321 and PR29322, in which ICEs occurred for the
! lack of proper attention to checking pointers in gfc_conv_function_call.
!
! Contributed by Olav Vahtras  <vahtras@pdc.kth.se>
! and Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
MODULE myint
   TYPE NUM
      INTEGER :: R = 0
   END TYPE NUM
   CONTAINS 
      FUNCTION FUNC(A,B) RESULT(E)
      IMPLICIT NONE
      TYPE(NUM)  A,B,E
      INTENT(IN) ::  A,B
      OPTIONAL B
      E%R=A%R
      CALL SUB(A,E)
      END FUNCTION FUNC

      SUBROUTINE SUB(A,E,B,C)
      IMPLICIT NONE
      TYPE(NUM) A,E,B,C
      INTENT(IN)   A,B
      INTENT(OUT)  E,C
      OPTIONAL B,C
      E%R=A%R
      END SUBROUTINE SUB
END MODULE myint

  if (isscan () /= 0) call abort
contains
  integer function isscan (substr)
    character(*), optional :: substr
    if (.not.present(substr)) isscan = myscan ("foo", "over")
  end function isscan
end
