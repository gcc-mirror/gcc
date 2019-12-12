! { dg-do run }
! PR 79485 - used to crash because the wrong routine was called.
module fmod1

  contains

  subroutine foo(i)
    implicit none

    integer, intent(inout) :: i

    i=i+1

  end subroutine foo

end module fmod1

module fmod2
  use iso_c_binding
  use fmod1, only : foo_first => foo

  contains

  subroutine foo(i) bind(c)
    implicit none

    integer, intent(inout) :: i

    i=i+2
    call foo_first(i)

  end subroutine foo

end module fmod2

  use fmod2
  
  call foo(i)
end
