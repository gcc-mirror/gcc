! { dg-do compile }
! Tests the fix for PR32236, in which the error below manifested itself
! as an ICE.
! Contributed by Bob Arduini <r.f.arduini@larc.nasa.gov>
  real :: x(2) = 1.0 ! { dg-error "already is initialized" }
  data x /1.0, 2.0/  ! { dg-error "already is initialized" }
  print *, x
end
