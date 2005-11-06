! { dg-do run }
! { dg-require-effective-target fortran_large_real }
! PR 24174 and PR 24305
program large_real_kind_form_io_1
  ! This should be 10 on systems that support kind=10
  integer, parameter :: k = selected_real_kind (precision (0.0_8) + 1)
  real(kind=k) :: a,b(2), c, eps
  complex(kind=k) :: d, e, f(2), g
  character(len=180) :: tmp
  ! Test real(k) scalar and array formatted IO
  eps = 10 * spacing (2.0_k) ! 10 ulp precision is enough.
  b(:) = 2.0_k
  write (tmp, *) b
  read (tmp, *) a, c
  if (abs (a - b(1)) > eps) call abort ()
  if (abs (c - b(2)) > eps) call abort ()
  ! Complex(k) scalar and array formatted and list formatted IO
  d = cmplx ( 1.0_k, 2.0_k, k)
  f = d
  write (tmp, *) f
  read (tmp, *) e, g
  if (abs (e - d) > eps) call abort ()
  if (abs (g - d) > eps) call abort ()
  write (tmp, '(2(e12.4e5, 2x))') d
  read (tmp, '(2(e12.4e5, 2x))') e
  if (abs (e - d) > eps) call abort()
end program large_real_kind_form_io_1
