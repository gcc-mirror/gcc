! { dg-do compile }
!
! PR 60450: [4.7/4.8 Regression] ICE with SHAPE intrinsic
!
! Contributed by Dave Allured <dave.allured@noaa.gov>

  real, allocatable :: x(:,:)
  allocate (x(3,2), source=99.)
  print *, shape (x / 10.0)
end
