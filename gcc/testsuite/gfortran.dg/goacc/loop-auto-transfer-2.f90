! Verify that Graphite's analysis of the CFG loops gets correctly
! transferred to the OpenACC loop structure for loop-nests of depth 1

! { dg-additional-options "-fdump-tree-graphite-details -fdump-tree-oaccloops1-details -fopt-info-optimized -fopt-info-missed" }
! { dg-additional-options "--param max-isl-operations=0" }
! { dg-additional-options "-O2" }
! { dg-prune-output ".*not inlinable.*" }

module test_module

  real, allocatable :: array1(:)
  real, allocatable :: array2(:)

  contains

subroutine test_loop_nest_depth_1 ()
  implicit none

  integer :: i,n

  if (size (array1) /= size (array2)) return
  n = size(array1)

  !$acc parallel loop auto copy(array1, array2) ! { dg-message "assigned OpenACC gang vector loop parallelism" }
  ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-1 }
  ! { dg-message ".auto. loop can be parallel" "" {target *-*-*} .-2 }
  do i=1, n
     array2(i) = array1(i) ! { dg-message "loop has no data-dependences" }
  end do


  !$acc parallel loop auto copy(array1, array2) ! { dg-message "assigned OpenACC seq loop parallelism" }
  ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-1 }
  ! { dg-message "'auto' loop cannot be parallel" "" {target *-*-*} .-2 }
  do i=1, n-1
     array1(i+1) = array1(i) + 10 ! { dg-message "loop has data-dependences" }
     array2(i) = array1(i)
  end do

  return
end subroutine test_loop_nest_depth_1



end module test_module

! { dg-final { scan-tree-dump-times "number of SCoPs: 1" 2 "graphite" } }
