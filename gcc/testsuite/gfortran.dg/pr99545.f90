! { dg-do compile }
! { dg-options "-fcheck=mem" }
!
! Test the fix for PR99545, in which the allocate statements caused an ICE.
!
! Contributed by Juergen Reuter  <juergen.reuter@desy.de>
!
module commands
  implicit none
  private

  type, abstract :: range_t
     integer :: step_mode = 0
     integer :: n_step = 0
  end type range_t

  type, extends (range_t) :: range_int_t
     integer :: i_step = 0
  end type range_int_t

  type, extends (range_t) :: range_real_t
     real :: lr_step = 0
end type range_real_t

  type :: cmd_scan_t
     private
     class(range_t), dimension(:), allocatable :: range
   contains
     procedure :: compile => cmd_scan_compile
  end type cmd_scan_t

contains

  subroutine cmd_scan_compile (cmd)
    class(cmd_scan_t), intent(inout) :: cmd
    allocate (range_int_t :: cmd%range (3))
    allocate (range_real_t :: cmd%range (3))
  end subroutine cmd_scan_compile

end module commands
