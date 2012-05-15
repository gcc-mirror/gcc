! { dg-do compile }
!
! PR fortran/46328
!
! Contributed by Damian Rouson
!
module foo_module
  type ,abstract :: foo
  contains
    procedure(t_interface) ,deferred :: t
    procedure(assign_interface) ,deferred :: assign
    procedure(multiply_interface) ,deferred :: multiply
    generic :: operator(*) => multiply
    generic :: assignment(=) => assign
  end type
  abstract interface
    function t_interface(this)
      import :: foo 
      class(foo) :: this
      class(foo), allocatable ::t_interface
    end function 
    function multiply_interface(lhs,rhs) 
      import :: foo 
      class(foo), allocatable :: multiply_interface
      class(foo), intent(in) :: lhs
      real, intent(in) :: rhs
    end function 
    subroutine assign_interface(lhs,rhs) 
      import :: foo 
      class(foo), intent(in) :: rhs
      class(foo), intent(inout) :: lhs
    end subroutine 
  end interface
contains
  subroutine bar(x,dt)    
    class(foo) :: x
    real, intent(in) :: dt     
    x = x%t()*dt
  end subroutine 
end module
