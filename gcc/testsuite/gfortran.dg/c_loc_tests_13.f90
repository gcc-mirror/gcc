! { dg-do compile }
!
! PR fortran/38536  
! Consecutive array and substring references rejected as C_LOC argument
!
! contributed by Scot Breitenfield <brtnfld@hdfgroup.org>

  USE ISO_C_BINDING
  TYPE test
     CHARACTER(LEN=2), DIMENSION(1:2) :: c
  END TYPE test
  TYPE(test), TARGET :: chrScalar
  TYPE(C_PTR) :: f_ptr

  f_ptr = C_LOC(chrScalar%c(1)(1:1))
  END
