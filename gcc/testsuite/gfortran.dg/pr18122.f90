! { dg-do run { target fd_truncate } }
! test namelist with scalars and arrays.
! Based on example provided by thomas.koenig@online.de

program sechs_w
  implicit none

  integer, parameter :: dr=selected_real_kind(15)

  integer, parameter :: nkmax=6
  real (kind=dr) :: rb(nkmax)
  integer :: z

  real (kind=dr) :: dg
  real (kind=dr) :: a
  real (kind=dr) :: da
  real (kind=dr) :: delta
  real (kind=dr) :: s,t
  integer :: nk
  real (kind=dr) alpha0

  real (kind=dr) :: phi, phi0, rad, rex, zk, z0, drdphi, dzdphi

  namelist /schnecke/ z, dg, a, t, delta, s, nk, rb, alpha0

  open (10,status="scratch")
  write (10, *)  "&SCHNECKE"
  write (10, *)    " z=1,"
  write (10, *)    " dg=58.4,"
  write (10, *)    " a=48.,"
  write (10, *)    " delta=0.4,"
  write (10, *)    " s=0.4,"
  write (10, *)    " nk=6,"
  write (10, *)    " rb=60, 0, 40,"
  write (10, *)    " alpha0=20.,"
  write (10, *)    "/"

  rewind (10)
  read (10,schnecke)
  close (10)
  if ((z /= 1)       .or. (dg /= 58.4_dr)  .or. (a /= 48.0_dr)   .or. &
    (delta /= 0.4_dr).or. (s /= 0.4_dr)    .or. (nk /= 6)        .or. &
    (rb(1) /= 60._dr).or. (rb(2) /= 0.0_dr).or. (rb(3) /=40.0_dr).or. &
    (alpha0 /= 20.0_dr)) call abort ()
end program sechs_w
