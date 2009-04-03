! { dg-do compile }
!
! PR fortran/39594
!
! Contributed by Peter Knowles and reduced by Jakub Jelinek.
!
module pr39594
  implicit double precision(z)
  common /z/ z0,z1,z2,z3,z4,z5,z6,z7
contains
  subroutine foo
    implicit double precision(z)
    common /z/ z0,z1,z2,z3,z4,z5,z6,z7
    call bar(z0)
  end subroutine foo
end module

! { dg-final { cleanup-modules "pr39594" } }
