! { dg-do run }
!
! PR fortran/38669
! Loop bounds temporaries used before being defined for elemental subroutines
!
! Original testcase by Harald Anlauf <anlauf@gmx.de>

program gfcbu84_main
  implicit none
  integer           :: jplev, k_lev
  integer :: p(42)
  real    :: r(42)
  integer, pointer :: q(:)
  jplev = 42
  k_lev = 1
  call random_number (r)
  p = 41 * r + 1
  allocate (q(jplev))

  q = 0
  call tq_tvgh (q(k_lev:), p(k_lev:))
  if (any (p /= q)) call abort

  q = 0
  call tq_tvgh (q(k_lev:), (p(k_lev:)))
  if (any (p /= q)) call abort

  q = 0
  call tq_tvgh (q(k_lev:), (p(p(k_lev:))))
  if (any (p(p) /= q)) call abort

  deallocate (q)

  contains
  elemental subroutine tq_tvgh (t, p)
    integer ,intent (out)            :: t
    integer ,intent (in)             :: p
    t=p
  end subroutine tq_tvgh
end program gfcbu84_main
