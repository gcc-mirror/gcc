! { dg-do run }

module m
  implicit none(type, external)

  logical :: is_present
  logical :: is_allocated
  integer :: has_value

contains

  subroutine test(a)
    integer, allocatable :: a
    call sub_val(a)
  end subroutine test

  subroutine test2(a)
    integer, allocatable, optional :: a
    call sub_val(a)
  end subroutine test2

  subroutine sub_val(x)
    integer, optional, value :: x
    if (present(x) .neqv. (is_present .and. is_allocated)) stop 1
    if (present(x)) then
      if (x /= has_value) stop 2
    end if
  end subroutine sub_val

end module m

use m
implicit none(type, external)
integer, allocatable :: b

is_allocated = .false.
is_present = .false.
call test2()

is_present = .true.
call test(b)
call test2(b)

b = 4
is_allocated = .true.
has_value = b
call test(b)
call test2(b)
deallocate(b)

end program
