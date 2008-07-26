! { dg-do compile }
! PR 36934 - this used to give a spurious error and segfault with a
! patch that wasn't complete
! Test case contributed by Philip Mason

module fred1
real, allocatable :: default_clocks(:)
end module fred1

module fred2
real, allocatable :: locks(:)
end module fred2

program fred
use fred1
use fred2
end program fred
! { dg-final { cleanup-modules "fred1 fred2" } }
