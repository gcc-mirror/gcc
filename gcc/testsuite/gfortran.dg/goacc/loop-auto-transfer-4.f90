! Verify that Graphite's analysis of the CFG loops gets correctly
! transferred to the OpenACC loop structure for loop-nests of depth 3

! { dg-additional-options "-fdump-tree-graphite-details -fdump-tree-oaccloops1-details" }
! { dg-additional-options "-fopt-info-optimized -fopt-info-missed" }
! { dg-additional-options "-O2" }
! { dg-prune-output ".*not inlinable.*" }

module test_module
  implicit none

  integer, parameter :: n = 100

contains

  subroutine test_loop_nest_depth_3 (array)
    integer :: i, j, k
    real :: array (2, n, n, n)

    ! Perfect loop-nest. Can be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC worker loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n
             array (1, i, j, k) = array(2, i, j, k) ! { dg-message "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel

    ! Perfect loop-nest. Innermost loop cannot be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (1, i, j, k+1) = array(1, i, j, k) ! { dg-message "loop has data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Perfect loop-nest. Cannot be parallel because it contains no
    ! data-reference and is hence not analyzed by Graphite. This is
    ! expected: empty loops should not be parallel either cf. e.g.
    ! "../../gfortran.dg/goacc/note-parallelism.f90".

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-missed ".auto. loop has not been analyzed .cf. .graphite. dumps for more information.." "" {target *-*-*} .-2 }
    do i=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-missed ".auto. loop has not been analyzed .cf. .graphite. dumps for more information.." "" {target *-*-*} .-2 }
       do j=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-bogus "loop has no data-dependences" "OpenACC internal chunking CFG loop not analyzed" {target *-*-*} .-2 }
       ! { dg-missed ".auto. loop has not been analyzed .cf. .graphite. dumps for more information.." "" {target *-*-*} .-3 }
          do k=1, n
             array (1, i, j, k) = array(1, i, j, k) ! { dg-bogus "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. All levels can be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       array (2, i, n, n) = array (1, i, n, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC worker loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n-1
          array (2, i, j, n) = array (1, i, j, n) ! { dg-message "loop has no data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (2, i, j, k) = array(1, i, j, k) ! { dg-message "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level can be parallel, second level
    ! can be parallel, third level cannot be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       array (2, i, n, n) = array (1, i, n, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n-1
          array (2, i, j, n) = array (1, i, j, n) ! { dg-message "loop has no data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (1, i, j, k+1) = array(1, i, j, k) ! { dg-message "loop has data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level can be parallel, second level
    ! cannot be parallel, third level can be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       array (2, i, n, n) = array (1, i, n, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n-1
          array (1, i, j+1, n) = array (1, i, j, n) ! { dg-message "loop has data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (2, i, j, k) = array(1, i, j, k) ! { dg-message "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level can be parallel, second and
    ! third level cannot be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC gang vector loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n
       array (2, i, n, n) = array (1, i, n, n) ! { dg-message "loop has no data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n-1
          array (1, i, j+1, n) = array (1, i, j, n) ! { dg-message "loop has data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (1, i, j, k+1) = array(1, i, j, k) ! { dg-message "loop has data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level cannot be parallel, second and
    ! third levels can be parallel

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n - 1
       array (1, i+1, 1, 1) = array (1, i, 1, 1) ! { dg-message "loop has data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC gang worker loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n
             array (1, i, j, k) = array(2, i, j, k) ! { dg-message "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level cannot be parallel, second
    ! level can be parallel, third level cannot be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n - 1
       array (1, i+1, 1, 1) = array (1, i, 1, 1) ! { dg-message "loop has data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC gang vector loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n - 1
             array (1, i, j, k+1) = array(1, i, j, k) ! { dg-message "loop has data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. First level cannot be parallel, second
    ! level cannot be parallel, third level can be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n - 1
       array (1, i+1, 1, 1) = array (1, i, 1, 1) ! { dg-message "loop has data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n - 1
          array (1, i, j+1, 1) = array (1, i, j, 1) ! { dg-message "loop has data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC gang vector loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n
             array (1, i, j, k) = array(2, i, j, k) ! { dg-message "loop has no data-dependences" }
          end do
       end do
    end do
    !$acc end parallel


    ! Imperfect loop-nest. All levels cannot be parallel.

    !$acc parallel copy(array)
    !$acc loop auto
    ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
    ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
    do i=1, n-1
       array (1, i+1, 1, 1) = array (1, i, 1, 1) ! { dg-message "loop has data-dependences" }
       !$acc loop auto
       ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
       ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
       do j=1, n-1
          array (1, i, j+1, 1) = array (1, i, j, 1) ! { dg-message "loop has data-dependences" }
          !$acc loop auto
          ! { dg-message "assigned OpenACC seq loop parallelism" "" {target *-*-*} .-1 }
          ! { dg-message "loop has no data-dependences" "OpenACC internal chunking CFG loop can be parallel" {target *-*-*} .-2 }
          do k=1, n-1
             array (1, i, j, k+1) = array(1, i, j, k) ! { dg-message "loop has data-dependences" }
          end do
       end do
    end do
    !$acc end parallel

    return
  end subroutine test_loop_nest_depth_3

end module test_module


!  Outlined functions for all kernels but the one without data-references should be analyzed.
! { dg-final { scan-tree-dump-times "number of SCoPs: 1" 10 "graphite"  } }
! Original test functon and one outlined kernel function should not be analyzed
! { dg-final { scan-tree-dump-times "number of SCoPs: 0" 2 "graphite" } }
