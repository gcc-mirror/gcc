! { dg-do run }
!
! Test the fix for PR104272 in which allocate caused an unwanted finalization
!
! Contributed by Kai Germaschewski  <kai.germaschewski@gmail.com>
!
module solver_m
    implicit none

    type, abstract, public :: solver_base_t
    end type solver_base_t

    type, public, extends(solver_base_t) :: solver_gpu_t
       complex, dimension(:), allocatable :: x
    contains
       final :: solver_gpu_final
    end type solver_gpu_t

    type, public, extends(solver_gpu_t) :: solver_sparse_gpu_t
    contains
       final :: solver_sparse_gpu_final
    end type solver_sparse_gpu_t

    integer :: final_counts = 0

 contains

    impure elemental subroutine solver_gpu_final(this)
       type(solver_gpu_t), intent(INOUT) :: this
       final_counts = final_counts + 1
    end subroutine solver_gpu_final

    impure elemental subroutine solver_sparse_gpu_final(this)
       type(solver_sparse_gpu_t), intent(INOUT) :: this
       final_counts = final_counts + 10
    end subroutine solver_sparse_gpu_final

 end module solver_m

 subroutine test
    use solver_m
    implicit none

    class(solver_base_t), dimension(:), allocatable :: solver

    allocate(solver_sparse_gpu_t :: solver(2))

    if (final_counts .ne. 0) stop 1
 end subroutine

program main
    use solver_m
    implicit none

    call test
    if (final_counts .ne. 22) stop 2 ! Scalar finalizers for rank 1/size 2
end program
