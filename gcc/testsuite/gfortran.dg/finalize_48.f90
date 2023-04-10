! { dg-do run }
!
! Check that pr106576 is fixed. The temporary from the function result
! was not being finalized.
!
! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>
!
module y
  implicit none
  type foo
     integer :: n
   contains
     final :: cleanup
  end type foo
  interface assignment (=)
     module procedure assign
  end interface assignment (=)
  character(16) :: buffer(4)
  integer :: buffer_count = 1
contains

  subroutine assign (rop, op)
    type(foo), intent(inout) :: rop
    type(foo), intent(in) :: op
    rop%n = op%n + 1
    write (buffer(buffer_count), '(A12,I4)') "assign", rop%n
    buffer_count = buffer_count + 1
  end subroutine assign

  function to_foo(n) result(res)
    integer, intent(in) :: n
    type (foo) :: res
    res%n = n
    write (buffer(buffer_count),  '(A12,I4)') "to_foo", res%n
    buffer_count = buffer_count + 1
  end function to_foo

  subroutine cleanup (self)
    type (foo), intent(inout) :: self
    write (buffer(buffer_count),  '(A12,I4)') "cleanup", self%n
    buffer_count = buffer_count + 1
  end subroutine cleanup
end module y

program memain
  use y
  implicit none
  character(16) :: check(4) = ["      to_foo   3", &
                               "      assign   4", &
                               "     cleanup   3", &
                               "     cleanup   4"]
  call chk
  if (any (buffer .ne. check)) stop 1
contains
  subroutine chk
    type (foo) :: a
    a = to_foo(3)
  end subroutine chk
end program memain
