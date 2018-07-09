! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Test the fix for PR49630, comment #11.
!
! Contributed by Vittorio Zecca  <zeccav@gmail.com>
!
module abc
  implicit none
  type,abstract::abc_abstract
  contains
    procedure(abc_interface),deferred::abc_function
  end type abc_abstract
  type,extends(abc_abstract)::abc_type
  contains
    procedure::abc_function
  end type abc_type
  abstract interface
    function abc_interface(this) ! { dg-error "assumed character length result" }
      import abc_abstract
      class(abc_abstract),intent(in)::this
      character(len=*)::abc_interface
    end function abc_interface
  end interface
contains
  function abc_function(this)
    class(abc_type),intent(in)::this
    character(len=5)::abc_function
    abc_function="hello"
  end function abc_function
  subroutine do_something(this)
    class(abc_abstract),intent(in)::this
    print *,this%abc_function()
  end subroutine do_something
end module abc
