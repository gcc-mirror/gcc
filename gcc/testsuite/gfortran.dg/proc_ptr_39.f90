! { dg-do compile }
!
! PR 52909: [F03] Procedure pointers not private to modules
!
! Contributed by Andrew Benson <abenson@caltech.edu>

module Module1
  procedure(), pointer, private :: procPtr => null()
end module

module Module2
  procedure(), pointer, private :: procPtr => null()
end module

program Test
  use Module1
  use Module2
end program

! { dg-final { cleanup-modules "Module1 Module2" } }
