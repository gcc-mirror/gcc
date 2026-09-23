! { dg-do run }
!
! Verify that an associate-name whose target is a call to an internal
! procedure (CONTAINS in a program) gets the correct declared kind from
! the function's return type, instead of falling back to default kind.
!
program demo
  use, intrinsic :: iso_fortran_env, only: wp => real64
  implicit none
  complex(wp) :: z
  real(wp) :: re_ref, im_ref

  z = (1.0_wp, 2.0_wp)
  re_ref = real (sin (z), wp)
  im_ref = aimag (sin (z))

  associate (k => myfunc (z))
    if (kind (k%re) /= kind (1.0_wp)) stop 1
    if (kind (k%im) /= kind (1.0_wp)) stop 2
    if (kind (aimag (k)) /= kind (1.0_wp)) stop 3
    if (abs (k%re - re_ref) > 1.0e-12_wp) stop 4
    if (abs (k%im - im_ref) > 1.0e-12_wp) stop 5
    if (abs (aimag (k) - im_ref) > 1.0e-12_wp) stop 6
  end associate

contains

  complex(wp) function myfunc (x)
    complex(wp), intent(in) :: x
    myfunc = sin (x)
  end function myfunc

end program demo

