! { dg-do run }
! PR fortran/61615 - resolve correct generic with TYPE(C_PTR) arguments
! PR fortran/99982 - dto. with C_PTR and C_FUNPTR
! Contributed by Jacob Abel and Scot Breitenfeld

MODULE foo
  USE iso_c_binding, only : c_ptr, c_funptr
  IMPLICIT NONE
  integer      :: rank = -99
  character(8) :: ctyp = ""
  INTERFACE bar
    MODULE PROCEDURE bar_s
    MODULE PROCEDURE bar_a1d
    MODULE PROCEDURE bar_a2d
    MODULE PROCEDURE bar_fp
    MODULE PROCEDURE bar_fp1
    MODULE PROCEDURE bar_fpx
  END INTERFACE bar
CONTAINS
  SUBROUTINE bar_s(a)
    TYPE(c_ptr) :: a
    WRITE (0, *) 'in bar_s'
    rank = 0
    ctyp = "c_ptr"
  END SUBROUTINE bar_s

  SUBROUTINE bar_a1d(a)
    TYPE(c_ptr) :: a(:)
    WRITE (0, *) 'in bar_a1d'
    rank = 1
    ctyp = "c_ptr"
  END SUBROUTINE bar_a1d

  SUBROUTINE bar_a2d(a)
    TYPE(c_ptr) :: a(:,:)
    WRITE (0, *) 'in bar_a2d'
    rank = 2
    ctyp = "c_ptr"
  END SUBROUTINE bar_a2d

  SUBROUTINE bar_fp(a)
    TYPE(c_funptr) :: a
    WRITE (0, *) 'in bar_fp'
    rank = 0
    ctyp = "c_funptr"
  END SUBROUTINE bar_fp

  SUBROUTINE bar_fp1(a)
    TYPE(c_funptr) :: a(:)
    WRITE (0, *) 'in bar_fp1'
    rank = 1
    ctyp = "c_funptr"
  END SUBROUTINE bar_fp1

  SUBROUTINE bar_fpx(a, b)
    TYPE(c_funptr) :: a(..)
    TYPE(c_ptr)    :: b
    WRITE (0, *) 'in bar_fpx'
    rank = -1
    ctyp = "c_funptr"
  END SUBROUTINE bar_fpx
END MODULE foo

PROGRAM cptr_array_vs_scalar_arg
  USE foo
  USE iso_c_binding, only : c_ptr, c_loc, c_funptr
  IMPLICIT NONE
  INTEGER, TARGET :: i
  TYPE(c_ptr)     :: a, b(1), c(1,1)
  type(c_funptr)  :: fp, fp1(1), fp2(1,1)
  a    = C_LOC(i)
  b(1) = C_LOC(i)
  CALL bar(a)
  if (rank /= 0 .or. ctyp /= "c_ptr") stop 1
  CALL bar(b)
  if (rank /= 1 .or. ctyp /= "c_ptr") stop 2
  CALL bar(c)
  if (rank /= 2 .or. ctyp /= "c_ptr") stop 3
  rank = -99
  ctyp = ""
  CALL bar((a))
  if (rank /= 0 .or. ctyp /= "c_ptr") stop 4
  CALL bar((b))
  if (rank /= 1 .or. ctyp /= "c_ptr") stop 5
  rank = -99
  ctyp = ""
  CALL bar(fp)
  if (rank /= 0 .or. ctyp /= "c_funptr") stop 6
  CALL bar(fp1)
  if (rank /= 1 .or. ctyp /= "c_funptr") stop 7
  rank = -99
  ctyp = ""
  CALL bar(fp2, a)
  if (rank /= -1 .or. ctyp /= "c_funptr") stop 8
END PROGRAM cptr_array_vs_scalar_arg
