! { dg-do compile }
! PR44568 - class array impelementation.
!
! Contributed by Hans-Werner Boschmann
!
module ice6

  type::a_type
   contains
     procedure::do_something
  end type a_type

  contains

  subroutine do_something(this)
    class(a_type),intent(in)::this
  end subroutine do_something

  subroutine do_something_else()
    class(a_type),dimension(:),allocatable::values
    call values(1)%do_something()
  end subroutine do_something_else

end module ice6
