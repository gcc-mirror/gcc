! { dg-do run }
! { dg-options "-std=legacy" }
!
! PR18218
! The IO library has an algorithm that involved repeated multiplication by 10,
! resulting in introducing large cumulative floating point errors.
program foo
  character*20 s
  real(kind=8) d
  s = "-.18774312893273    "
  read(unit=s, fmt='(g20.14)') d
  if (d + 0.18774312893273d0 .gt. 1d-13) call abort
end program

