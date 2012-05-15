! { dg-do compile }
! { dg-options "-std=gnu" }

module module_implicit_conversion
  ! double complex :: s = (1.0D0, 0D0) 
  double complex :: s = (1.0, 0D0)
end module module_implicit_conversion
