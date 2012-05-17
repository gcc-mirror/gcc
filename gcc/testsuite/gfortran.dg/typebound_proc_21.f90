! { dg-do compile }
!
! PR fortran/47455
!
module class_t
    type :: tx
        integer, dimension(:), allocatable :: i
    end type tx
    type :: t
        type(tx), pointer :: x
    contains
        procedure :: calc
        procedure :: find_x
    end type t
contains
    subroutine calc(this)
        class(t), target :: this
        this%x = this%find_x()
    end subroutine calc
    function find_x(this)
        class(t), intent(in) :: this
        type(tx), pointer :: find_x
        find_x => null()
    end function find_x
end module class_t
