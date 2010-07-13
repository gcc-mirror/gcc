! { dg-do compile }
!
! PR 44565: [4.6 Regression] [OOP] ICE in gimplify_expr with array-valued generic TBP
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module ice6

  type :: t
   contains
     procedure :: get_array
     generic :: get_something => get_array
  end type

contains

  function get_array(this)
    class(t) :: this
    real,dimension(2) :: get_array
  end function get_array

  subroutine do_something(this)
    class(t) :: this
    print *,this%get_something()
  end subroutine do_something

end module ice6 

! { dg-final { cleanup-modules "ice6" } }
