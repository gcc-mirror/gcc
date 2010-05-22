! { dg-do compile }
!
! PR 44212: [OOP] ICE when defining a pointer component before defining the class and calling a TBP then
!
! Contributed by Hans-Werner Boschmann <boschmann@tp1.physik.uni-siegen.de>

module ice_module

  type :: B_type
     class(A_type),pointer :: A_comp
  end type B_type

  type :: A_type
  contains
     procedure :: A_proc
  end type A_type

contains

  subroutine A_proc(this)
    class(A_type),target,intent(inout) :: this
  end subroutine A_proc

  subroutine ice_proc(this)
    class(A_type) :: this
    call this%A_proc()
  end subroutine ice_proc

end module ice_module

! { dg-final { cleanup-modules "ice_module" } }
