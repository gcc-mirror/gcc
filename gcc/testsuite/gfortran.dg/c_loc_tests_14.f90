! { dg-do compile }
!
! PR fortran/38536
! Accept as argument to C_LOC a subcomponent accessed through a pointer.

  USE ISO_C_BINDING

  IMPLICIT NONE
  TYPE test3
          INTEGER, DIMENSION(5) :: b
  END TYPE test3

  TYPE test2
          TYPE(test3), DIMENSION(:), POINTER :: a
  END TYPE test2

  TYPE test
          TYPE(test2), DIMENSION(2) :: c
  END TYPE test

  TYPE(test) :: chrScalar
  TYPE(C_PTR) :: f_ptr
  TYPE(test3), TARGET :: d(3)


  chrScalar%c(1)%a => d
  f_ptr = C_LOC(chrScalar%c(1)%a(1)%b(1))
  end

