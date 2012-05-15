! { dg-do compile }
! PR fortran/36316
!
! gfortran generated a mismatching tree ("type mismatch in binary expression")
! for array bounds (mixing integer kind=4/kind=8 without fold_convert).
!
MODULE YOMCAIN

IMPLICIT NONE
SAVE

TYPE distributed_vector
REAL, pointer :: local(:)
INTEGER(4)       :: global_length,local_start
INTEGER(8)       :: local_end
END TYPE distributed_vector

INTERFACE ASSIGNMENT (=)
MODULE PROCEDURE assign_ar_dv
END INTERFACE

INTERFACE OPERATOR (*)
MODULE PROCEDURE multiply_dv_dv
END INTERFACE

CONTAINS

SUBROUTINE assign_ar_dv (handle,pvec)

!         copy array to the distributed_vector

REAL,                      INTENT(IN)    :: pvec(:)
TYPE (distributed_vector), INTENT(INOUT) :: handle

handle%local(:) = pvec(:)

RETURN
END SUBROUTINE assign_ar_dv

FUNCTION multiply_dv_dv (handle1,handle2)

!         multiply two distributed_vectors

TYPE (distributed_vector), INTENT(IN) :: handle2
TYPE (distributed_vector), INTENT(IN) :: handle1
REAL :: multiply_dv_dv(handle1%local_start:handle1%local_end)

multiply_dv_dv = handle1%local(:) * handle2%local(:)

RETURN
END FUNCTION multiply_dv_dv


SUBROUTINE CAININAD_SCALE_DISTVEC ()
TYPE (distributed_vector) :: PVAZG
TYPE (distributed_vector) :: ZTEMP
TYPE (distributed_vector) :: SCALP_DV

ZTEMP = PVAZG * SCALP_DV
END SUBROUTINE CAININAD_SCALE_DISTVEC
END MODULE YOMCAIN
