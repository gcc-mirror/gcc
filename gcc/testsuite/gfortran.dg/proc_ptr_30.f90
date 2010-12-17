! { dg-do compile }
!
! PR 46067: [F03] invalid procedure pointer assignment not detected
!
! Contributed by Stephen J. Bespalko <sjbespa@comcast.net>

  implicit none
  
  type test_type
    integer :: id = 1
  end type
  
  abstract interface
    real function fun_interface(t,x)
      import :: test_type
      real, intent(in) :: x
      class(test_type) :: t
    end function
  end interface  
  
  type(test_type) :: funs
  real :: r
  procedure(fun_interface), pointer :: pp

  pp => fun1        ! { dg-error "Interface mismatch in procedure pointer assignment" }
  r = pp(funs,0.)
  print *, " pp(0) ", r 

contains

  real function fun1 (t,x)
    real, intent(in) :: x
    type(test_type) :: t
    print *," id = ", t%id
    fun1 = cos(x)
  end function
 
end
