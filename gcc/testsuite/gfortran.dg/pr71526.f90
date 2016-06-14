! { dg-do link }
! { dg-require-effective-target lto }
! { dg-options "-O2 -flto" }

MODULE util
  INTERFACE sort
     MODULE PROCEDURE sort_cv
  END INTERFACE
CONTAINS
  SUBROUTINE sort_cv ( arr, n, index )
    CHARACTER(LEN=*), INTENT(INOUT)          :: arr(1:n)
    INTEGER, INTENT(OUT)                     :: INDEX(1:n)
    INTEGER, ALLOCATABLE, DIMENSION(:, :)    :: entries
    ALLOCATE(entries(max_length,SIZE(arr)))
  END SUBROUTINE sort_cv
END MODULE util
USE util
INTEGER, ALLOCATABLE :: ind(:)
character(len=3), ALLOCATABLE :: d(:)
CALL sort(d,N,ind)
END
