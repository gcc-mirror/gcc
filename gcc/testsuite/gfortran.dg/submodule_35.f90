! { dg-do run )
!
! Test the fix for prs 88632 and 104630, in which the private procedure
! 'my_number' was not visible in the submodule 'decendent'.
!
! Contributed by Joerg Stiller <joerg.stiller@tu-dresden.de>
!             and Neil Carlson  <neil.n.carlson@gmail.com>
!
module parent
  private
  public :: my_type
  
  type my_type
    integer :: i = 1
  contains
    procedure :: my_method
  end type my_type
  
  interface
    module subroutine my_method(this)
      class(my_type), intent(inout) :: this
    end subroutine my_method
  end interface
  
contains
  
  ! common helper function to be used in different submodules
  integer function my_number()
    my_number = 42
  end function my_number

end module parent
