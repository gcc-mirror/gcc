! { dg-do compile }
!
! PR 42051: [OOP] ICE on array-valued function with CLASS formal argument
!
! Original test case by Damian Rouson <damian@rouson.net>
! Modified by Janus Weil <janus@gcc.gnu.org>

  type grid
  end type 

contains

  function return_x(this) result(this_x)
    class(grid) :: this
    real  ,dimension(1) :: this_x
  end function

  subroutine output()
    type(grid) :: mesh
    real ,dimension(1) :: x
    x = return_x(mesh)
  end subroutine

end
