! { dg-do compile }
! { dg-options "-Wconversion-extra" }

  real(8) :: sqrt2
  real x

  x = 2.0
  sqrt2 = sqrt(x)      ! { dg-warning "Conversion" }

  sqrt2 = sqrt(2.0)    ! { dg-warning "Conversion" }
end
