! { dg-do compile }
! { dg-options "-std=f2008" }
! PR 49802
! Assumed-shape and explicit-shape array dummies may have the VALUE
! attribute since Fortran 2008 (F2008, C557), but assumed-size arrays
! may not.

subroutine foo (x)
  integer, value :: x(:)    ! assumed-shape: OK
end subroutine

subroutine bar (x)
  integer, value :: x(10)   ! explicit-shape: OK
end subroutine

subroutine baz (x) ! { dg-error "may not have the VALUE attribute" }
  integer, value :: x(*)
end subroutine
