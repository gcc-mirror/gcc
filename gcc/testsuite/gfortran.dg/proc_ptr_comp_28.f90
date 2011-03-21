! { dg-do compile }
!
! PR 47224: [F03] ICE with procedure pointer component
!
! Contributed by Martien Hulsen <m.a.hulsen@tue.nl>

  type coefficients_t
    procedure (real), pointer, nopass :: vfunc
  end type

  type(coefficients_t) :: coeff
  real, dimension(3) :: x

  print *, abs ( coeff%vfunc ( x(:) ) )

end
