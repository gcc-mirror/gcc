! { dg-do compile }
! { dg-options "-std=f95" }
abstract interface ! { dg-error "Fortran 2003: ABSTRACT INTERFACE" }
  subroutine two()
  end subroutine two
end interface ! { dg-error "Expecting END PROGRAM statement" }
end
