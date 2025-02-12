! { dg-do run { xfail long_double_is_ibm128 } }
! Test XFAILed on these platforms because the system's printf() lacks
! proper support for denormalized long doubles. See PR24685
! { dg-require-effective-target fortran_large_real }
! PR libfortran/24685
program large_real_kind_form_io_2
  ! This should be 10 or 16 on systems that support kind=10 or kind=16
  integer, parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(kind=k) :: a,b(2), c
  character(len=180) :: tmp

  b(:) = huge(0.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) STOP 1
  if (c /= b(2)) STOP 2

  b(:) = -huge(0.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) STOP 3
  if (c /= b(2)) STOP 4

  b(:) = nearest(tiny(0.0_k),1.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) STOP 5
  if (c /= b(2)) STOP 6

  b(:) = nearest(-tiny(0.0_k),-1.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) STOP 7
  if (c /= b(2)) STOP 8
end program large_real_kind_form_io_2
