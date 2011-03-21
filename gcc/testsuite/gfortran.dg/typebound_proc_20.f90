! { dg-do compile }
! TODO: make runtime testcase once bug is fixed
!
! PR fortran/47455
!
! Based on an example by Thomas Henlich
!

module class_t
    type :: tx
        integer, dimension(:), allocatable :: i
    end type tx
    type :: t
        type(tx), pointer :: x
        type(tx) :: y
    contains
        procedure :: calc
        procedure :: find_x
        procedure :: find_y
    end type t
contains
    subroutine calc(this)
        class(t), target :: this
        type(tx), target :: that
        that%i = [1,2]
        this%x => this%find_x(that, .true.)
        if (associated (this%x)) call abort()
        this%x => this%find_x(that, .false.)
        if(any (this%x%i /= [5, 7])) call abort()
        if (.not.associated (this%x,that)) call abort()
        allocate(this%x)
        if (associated (this%x,that)) call abort()
        if (allocated(this%x%i)) call abort()
        this%x = this%find_x(that, .false.)
        that%i = [3,4]
        if(any (this%x%i /= [5, 7])) call abort() ! FAILS

        if (allocated (this%y%i)) call abort()
        this%y = this%find_y()  ! FAILS
        if (.not.allocated (this%y%i)) call abort()
        if(any (this%y%i /= [6, 8])) call abort()
    end subroutine calc
    function find_x(this, that, l_null)
       class(t), intent(in) :: this
       type(tx), target  :: that
       type(tx), pointer :: find_x
       logical :: l_null
       if (l_null) then
         find_x => null()
       else
         find_x => that
         that%i = [5, 7]
       end if
    end function find_x
    function find_y(this) result(res)
        class(t), intent(in) :: this
        type(tx), allocatable :: res
        allocate(res)
        res%i = [6, 8]
   end function find_y
end module class_t

use class_t
type(t) :: x
call x%calc()
end

! { dg-final { cleanup-modules "class_t" } }
