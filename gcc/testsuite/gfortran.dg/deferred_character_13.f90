! { dg-do run }
!
! Tests the fix for PR49630 comment #3.
!
! Contributed by Janus Weil  <janus@gcc.gnu.org>
!
module abc
  implicit none

  type::abc_type
   contains
     procedure::abc_function
  end type abc_type

contains

  function abc_function(this)
    class(abc_type),intent(in)::this
    character(:),allocatable::abc_function
    allocate(abc_function,source="hello")
  end function abc_function

  subroutine do_something(this)
    class(abc_type),intent(in)::this
    if (this%abc_function() .ne. "hello") call abort
  end subroutine do_something

end module abc


  use abc
  type(abc_type) :: a
  call do_something(a)
end
