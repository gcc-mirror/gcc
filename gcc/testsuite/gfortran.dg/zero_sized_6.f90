! { dg-do compile }
! PR38709 - ICE-on-invalid on zero-sized array in init-expr.

  INTEGER, PARAMETER :: a(1) = (/ 1 /)
  INTEGER, PARAMETER :: i = a(shape(1))   ! { dg-error "Incompatible ranks" }
END
