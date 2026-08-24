! { dg-do compile }
! { dg-options "-std=f2003" }
! PR 49802
! Fortran 2003 C558 prohibited assumed-length character with VALUE.
! Verify that -std=f2003 rejects it.

subroutine sub (y)  ! { dg-error "Assumed-length character" }
  character(len=*), value :: y
end subroutine
