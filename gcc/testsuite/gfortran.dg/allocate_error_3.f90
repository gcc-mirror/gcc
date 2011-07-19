! { dg-do compile }
!
! PR 49708: [4.5/4.6/4.7 Regression] ICE with allocate and no dimensions
!
! Contributed by <fnordxyz@yahoo.com>

  real, pointer :: x(:)
  allocate(x)            ! { dg-error "Array specification required" }
end 
