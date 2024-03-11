! { dg-do compile }
! PR fortran/110826 - procedure pointer component in DT array

module m
  implicit none

  type pp
    procedure(func_template), pointer, nopass :: f =>null()
  end type pp

  abstract interface
     function func_template(state) result(dstate)
       implicit none
       real, dimension(:,:), intent(in)              :: state
       real, dimension(size(state,1), size(state,2)) :: dstate
     end function
  end interface

contains

  function zero_state(state) result(dstate)
    real, dimension(:,:), intent(in)              :: state
    real, dimension(size(state,1), size(state,2)) :: dstate
    dstate = 0.
  end function zero_state

end module m

program test_func_array
  use m
  implicit none

  real, dimension(4,6) :: state
  type(pp) :: func_scalar
  type(pp) :: func_array(4)

  func_scalar  %f => zero_state
  func_array(1)%f => zero_state
  print *, func_scalar  %f(state)
  print *, func_array(1)%f(state)
  if (.not. all (shape (func_scalar  %f(state)) == shape (state))) stop 1
  if (.not. all (shape (func_array(1)%f(state)) == shape (state))) stop 2
end program test_func_array
