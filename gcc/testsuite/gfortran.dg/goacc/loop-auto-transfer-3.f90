! Verify that Graphite's analysis of the CFG loops gets correctly
! transferred to the OpenACC loop structure for loop-nests of depth 2

! { dg-additional-options "-fdump-tree-graphite-details -fdump-tree-oaccloops1-details" }
! { dg-additional-options "-fopt-info-optimized -fopt-info-missed" }
! { dg-additional-options "-O2" }
! { dg-prune-output ".*not inlinable.*" }

module test_module
  implicit none

  integer, parameter :: n = 100
  integer, parameter :: m = 100

contains

  subroutine test_loop_nest_depth_2 (array)
    integer :: i, j
    real :: array (2, n, m)

    ! Perfect loop-nest, inner and outer loop can be parallel

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
    ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
    do i=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
       ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
       do j=1, m
          array (1, i, j) = array(2, i, j) ! { dg-message "loop has no data-dependences" }
       end do
    end do
    !$acc end parallel

    ! Imperfect loop-nest, inner and outer loop can be parallel

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
    ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
    do i=1, n
       array (2, i, n) = array(1, i, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
       ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
       do j=1, m
          array (1, i, j) = array (2, i,j) ! { dg-message "loop has no data-dependences" }
       end do
    end do
    !$acc end parallel

    ! Imperfect loop-nest, inner loop can be parallel, outer loop cannot be parallel

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking loop can be parallel" {target *-*-*} .-2 }
    ! { dg-message "'auto' loop cannot be parallel" "" {target *-*-*} .-3 }
    do i=1, n-1
       array (1, i+1, 1) = array (2, i, 1) ! { dg-message "loop has data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC gang vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
       ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
       do j=1, m
          array (1, i, j) = array (2, i, j) ! { dg-message "loop has no data-dependences" }
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest, inner loop can be parallel, outer loop cannot be parallel

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang vector loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "" {target *-*-*} .-2 }
    ! { dg-message "'auto' loop can be parallel" "" {target *-*-*} .-3 }
    do i=1, n
       array (2, i, n) = array (1, i, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking loop can be parallel" {target *-*-*} .-2 }
       ! { dg-message "'auto' loop cannot be parallel" "" {target *-*-*} .-3 }
       do j=1, m-1
          array (1, i, j+1) = array (1, i, j) ! { dg-message "loop has data-dependences" }
       end do
    end do
    !$acc end parallel
    return
  end subroutine test_loop_nest_depth_2

end module test_module


! { dg-final { scan-tree-dump-times "number of SCoPs: 1" 4 "graphite"  } } One function per kernel, all should be analyzed
! { dg-final { scan-tree-dump-times "number of SCoPs: 0" 1 "graphite" } } Original function should not be analyzed
