! { dg-do run { xfail spu-*-* } }
! FAILs on SPU because of rounding error reading kinds.h
program main
  ! Test scalar masks for different intrinsics.
  real, dimension(2,2) :: a
  logical(kind=2) :: lo
  lo = .false.
  a(1,1) = 1.
  a(1,2) = -1.
  a(2,1) = 13.
  a(2,2) = -31.
  if (any (minloc (a, lo) /= 0)) call abort
  if (any (minloc (a, .true.) /= (/ 2, 2 /))) call abort
  if (any (minloc(a, 1, .true.) /= (/ 1, 2/))) call abort
  if (any (minloc(a, 1, lo ) /= (/ 0, 0/))) call abort

  if (any (maxloc (a, lo) /= 0)) call abort
  if (any (maxloc (a, .true.) /= (/ 2,1 /))) call abort
  if (any (maxloc(a, 1, .true.) /= (/ 2, 1/))) call abort
  if (any (maxloc(a, 1, lo) /= (/ 0, 0/))) call abort

  if (any (maxval(a, 1, lo) /= -HUGE(a))) call abort
  if (any (maxval(a, 1, .true.) /= (/13., -1./))) call abort
  if (any (minval(a, 1, lo) /= HUGE(a))) call abort
  if (any (minval(a, 1, .true.) /= (/1., -31./))) call abort

  if (any (product(a, 1, .true.) /= (/13., 31./))) call abort
  if (any (product(a, 1, lo ) /= (/1., 1./))) call abort

  if (any (sum(a, 1, .true.) /= (/14., -32./))) call abort
  if (any (sum(a, 1, lo) /= (/0., 0./))) call abort

end program main
