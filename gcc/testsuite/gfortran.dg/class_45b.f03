! { dg-do link }
! { dg-additional-sources class_45a.f03 }
!
! PR 50227: [4.7 Regression] [OOP] ICE-on-valid with allocatable class variable
!
! Contributed by Andrew Benson <abenson@caltech.edu>

program Test
  use G_Nodes
  class(t0), allocatable :: c
  allocate(t1 :: c)
end program Test

! { dg-final { cleanup-modules "G_Nodes" } }
