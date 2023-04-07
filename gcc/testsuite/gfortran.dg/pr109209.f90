! { dg-do compile }
!
! Fix for a regression caused by
! r13-6747-gd7caf313525a46f200d7f5db1ba893f853774aee
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module resonances
  implicit none
  private

  type :: t1_t
     integer, dimension(:), allocatable :: c
   contains
     procedure, private :: t1_assign
     generic :: assignment(=) => t1_assign
  end type t1_t

  type :: t3_t
     type(t1_t), dimension(:), allocatable :: resonances
     integer :: n_resonances = 0
  contains
     procedure, private :: t3_assign
     generic :: assignment(=) => t3_assign
  end type t3_t

  type :: resonance_branch_t
     integer :: i = 0
     integer, dimension(:), allocatable :: r_child
     integer, dimension(:), allocatable :: o_child
  end type resonance_branch_t

  type :: resonance_tree_t
     private
     integer :: n = 0
     type(resonance_branch_t), dimension(:), allocatable :: branch
  end type resonance_tree_t

  type :: t3_set_t
     private
     type(t3_t), dimension(:), allocatable :: history
     type(resonance_tree_t), dimension(:), allocatable :: tree
     integer :: last = 0
   contains
     procedure, private :: expand => t3_set_expand
  end type t3_set_t

contains

  pure subroutine t1_assign &
       (t1_out, t1_in)
    class(t1_t), intent(inout) :: t1_out
    class(t1_t), intent(in) :: t1_in
    if (allocated (t1_out%c))  deallocate (t1_out%c)
    if (allocated (t1_in%c)) then
       allocate (t1_out%c (size (t1_in%c)))
       t1_out%c = t1_in%c
    end if
  end subroutine t1_assign

  subroutine t3_assign (res_hist_out, res_hist_in)
    class(t3_t), intent(out) :: res_hist_out
    class(t3_t), intent(in) :: res_hist_in
    if (allocated (res_hist_in%resonances)) then
       res_hist_out%resonances = res_hist_in%resonances
       res_hist_out%n_resonances = res_hist_in%n_resonances
    end if
  end subroutine t3_assign

  subroutine t3_set_expand (res_set)
    class(t3_set_t), intent(inout) :: res_set
    type(t3_t), dimension(:), allocatable :: history_new
    integer :: s
    s = size (res_set%history)
    allocate (history_new (2 * s))
    history_new(1:s) = res_set%history(1:s)
    call move_alloc (history_new, res_set%history)
  end subroutine t3_set_expand

end module resonances
