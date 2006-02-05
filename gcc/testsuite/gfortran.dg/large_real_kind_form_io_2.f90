! { dg-do run }
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
  if (a /= b(1)) call abort ()
  if (c /= b(2)) call abort ()

  b(:) = -huge(0.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) call abort ()
  if (c /= b(2)) call abort ()

  b(:) = tiny(0.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) call abort ()
  if (c /= b(2)) call abort ()

  b(:) = -tiny(0.0_k)
  write (tmp, *) b
  read (tmp, *) a, c
  if (a /= b(1)) call abort ()
  if (c /= b(2)) call abort ()
end program large_real_kind_form_io_2
