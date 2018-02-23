!{ dg-do run }
!
! Testcase for pr66927, pr67123
! Contributed by Juergen Reuter <juergen.reuter@desy.de>

module processes
  implicit none
  private

  type :: t1_t
     real :: p = 0.0
  end type t1_t

  type :: t2_t
     private
     type(t1_t), dimension(:), allocatable :: p
   contains
     procedure :: func => t2_func
  end type t2_t

  type, public :: t3_t
    type(t2_t), public :: int_born
  end type t3_t

  public :: evaluate

contains

  function t2_func (int) result (p)
    class(t2_t), intent(in) :: int
    class(t1_t), dimension(:), allocatable :: p
    allocate(p(5))
  end function t2_func

  subroutine evaluate (t3)
    class(t3_t), intent(inout) :: t3
    type(t1_t), dimension(:), allocatable :: p_born
    allocate (p_born(1:size(t3%int_born%func ())), &
         source = t3%int_born%func ())
    if (.not. allocated(p_born)) STOP 1
    if (size(p_born) /= 5) STOP 2
  end subroutine evaluate

end module processes

program pr66927
use processes
type(t3_t) :: o
call evaluate(o)
end

