! { dg-do "compile" }
! { dg-options "-Wconversion-extra" }

  real(8) :: sqrt2
  sqrt2 = sqrt(2.0)      ! { dg-warning "conversion" }
end
