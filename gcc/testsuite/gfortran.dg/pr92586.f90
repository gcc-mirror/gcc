! { dg-do compile }
!
! Contributed by Emanuele Pagone  <epagone@email.it>
!
module foo_m
  implicit none

  type :: string
    character(len=:), allocatable :: s
  end type string

  type :: foo_t
    type(string), allocatable :: foo_s(:)
  contains
    procedure, public :: get_s
  end type foo_t

  type :: data_t
    integer                   :: n_foo_s
    type(foo_t), allocatable  :: foo(:)
  contains
    procedure, public :: data_get_foo_s
  end type data_t

contains

  function get_s(self)
    class(foo_t), intent(in)  :: self
    type(string)  :: get_s( size(self%foo_s) )
    get_s = self%foo_s
  end function get_s

  function data_get_foo_s(self, ith)
    class(data_t), intent(in) :: self
    integer, intent(in)       :: ith
    type(string)              :: data_get_foo_s(self%n_foo_s)

    data_get_foo_s = self%foo(ith)%get_s() ! The lhs was not dereferenced in a byref call.

  end function data_get_foo_s

end module foo_m


program bug_stringifor
  use foo_m
  implicit none

  type(data_t)              :: data
  type(string), allocatable :: bar(:)

  allocate( data%foo(1) )
  data%foo(1)%foo_s = [string("alpha"), string("bravo"), string("charlie"), &
                        string("delta"), string("foxtrot")]
  data%n_foo_s = 5

  bar = data%data_get_foo_s(1)

  print *, "bar = ", bar(1)%s

end program bug_stringifor
