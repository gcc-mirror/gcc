! PR fortran/103691
! { dg-do compile }
! { dg-options "-O2 -g" }

program pr103691
  real, parameter :: a(0) = 2.0
  real, allocatable :: b(:)
  allocate (b, mold=a)
end
