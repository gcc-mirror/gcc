! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/38669
! Temporary created for pointer as actual argument of an elemental subroutine
!
! Original testcase by Harald Anlauf <anlauf@gmx.de>

program gfcbu84_main
  implicit none
  integer           :: jplev, k_lev
  real :: p(42)
  real, pointer :: q(:)
  jplev = 42
  k_lev = 1
  allocate (q(jplev))
  call tq_tvgh (q(k_lev:), p(k_lev:))
  deallocate (q)

  contains
  elemental subroutine tq_tvgh (t, p)
    real ,intent (out)            :: t
    real ,intent (in)             :: p
    t=p
  end subroutine tq_tvgh
end program gfcbu84_main
! { dg-final { scan-tree-dump-times "atmp" 0 "original" } }
