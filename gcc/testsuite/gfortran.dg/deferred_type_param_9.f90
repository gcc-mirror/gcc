! { dg-do run }
!
! PR fortran/57596
!
! Contributed by Valery Weber
!
PROGRAM main
  IMPLICIT NONE
  call get ()
  call get2 ()
contains
  SUBROUTINE get (c_val)
    CHARACTER( : ), INTENT( INOUT ), ALLOCATABLE, OPTIONAL :: c_val
    CHARACTER( 10 ) :: c_val_tmp
    if(present(c_val)) STOP 1
  END SUBROUTINE get
  SUBROUTINE get2 (c_val)
    CHARACTER( : ), INTENT( OUT ), ALLOCATABLE, OPTIONAL :: c_val
    CHARACTER( 10 ) :: c_val_tmp
    if(present(c_val)) STOP 2
  END SUBROUTINE get2
END PROGRAM main
