! { dg-do compile  { target skip-all-targets } }
!
! To be used by public_private_module_3.f90
!
! PR fortran/52916
! Cf. PR fortran/40973
!
! Ensure that PRIVATE specific functions do not get
! marked as TREE_PUBLIC() = 0, if the generic name is
! PUBLIC.
!
use m
use m2
implicit none

type(t) :: a, b, c
type(t2) :: x

call gen()
a = b + (c .myop. a)

call x%func()
end
