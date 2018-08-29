! { dg-do run }
! { dg-options "-Wno-underflow" }
! Check that the chopping of bits of subnormal numbers works.
!
program chop
  real x
  x = 1.
  if (tiny(x)/2. /= tiny(x)/2. - (nearest(tiny(x),1.) - tiny(x))/2.) then
    STOP 1
  end if
end program chop
