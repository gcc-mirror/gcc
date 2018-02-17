! { dg-do run }
!
! Test the fix for PR82550 in which the reference to 'p' in 'foo'
! was not being correctly handled.
!
! Contributed by Reinhold Bader  <Bader@lrz.de>
!
module m_subm_18_pos
  implicit none
  integer :: i = 0
  interface
    module subroutine foo(fun_ptr)
      procedure(p), pointer, intent(out) :: fun_ptr
    end subroutine
  end interface
contains
  subroutine p()
    i = 1
  end subroutine p
end module m_subm_18_pos
submodule (m_subm_18_pos) subm_18_pos
    implicit none
contains
    module subroutine foo(fun_ptr)
      procedure(p), pointer, intent(out) :: fun_ptr
      fun_ptr => p
    end subroutine
end submodule
program p_18_pos
  use m_subm_18_pos
  implicit none
  procedure(), pointer :: x
  call foo(x)
  call x()
  if (i == 1) then
     write(*,*) 'OK'
  else
     write(*,*) 'FAIL'
     STOP 1
  end if
end program p_18_pos

