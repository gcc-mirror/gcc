! { dg-do run }
!
! Test the fix for PR80524, where gfortran on issued one final call
! For 'u' going out of scope. Two further call should be emitted; one
! for the lhs of the assignment in 's' and the other for the function
! result, which occurs after assignment.
!
! Contributed by Andrew Wood  <andrew@fluidgravity.co.uk>
!
MODULE m1
   IMPLICIT NONE
   integer :: counter = 0
   integer :: fval = 0
   TYPE t
      INTEGER :: i
      CONTAINS
         FINAL :: t_final
   END TYPE t
   CONTAINS
      SUBROUTINE t_final(this)
         TYPE(t) :: this
         counter = counter + 1
      END SUBROUTINE
      FUNCTION new_t()
         TYPE(t) :: new_t
         new_t%i = 1
         fval = new_t%i
         if (counter /= 0) stop 1   ! Finalization of 'var' after evaluation of 'expr'
      END FUNCTION new_t
      SUBROUTINE s
         TYPE(t) :: u
         u = new_t()
         if (counter /= 2) stop 2   ! Finalization of 'var' and 'expr'
      END SUBROUTINE s
END MODULE m1
PROGRAM prog
   USE m1
   IMPLICIT NONE
   CALL s
   if (counter /= 3) stop 3         ! Finalization of 'u' in 's'
END PROGRAM prog
