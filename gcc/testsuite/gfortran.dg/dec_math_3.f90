! { dg-options "-std=gnu" }
! { dg-do compile }

real, parameter :: dacos = acosd(1.1) ! { dg-error "Argument of ACOSD at .1. must be within the closed interval \\\[-1, 1\\\]" }
print *, dacos
real, parameter :: dasin = asind(-1.1) ! { dg-error "Argument of ASIND at .1. must be within the closed interval \\\[-1, 1\\\]" }
print *, dasin
real, parameter :: datan2 = atan2d(0.0, 0.0) ! { dg-error "If the first argument of ATAN2D at .1. is zero, then the second argument must not be zero" }
print *, datan2
real, parameter :: piacos = acospi(-1.1) ! { dg-error "Argument of ACOSPI at .1. must be within the closed interval \\\[-1, 1\\\]" }
print *, piacos
real, parameter :: piasin = asinpi(1.1) ! { dg-error "Argument of ASINPI at .1. must be within the closed interval \\\[-1, 1\\\]" }
print *, piasin
real, parameter :: piatan2 = atan2pi(0.0, 0.0) ! { dg-error "If the first argument of ATAN2PI at .1. is zero, then the second argument must not be zero" }
print *, piatan2

end
