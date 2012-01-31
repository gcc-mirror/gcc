! { dg-do run }
! PR48705 - ALLOCATE with class function expression for SOURCE failed.
! This is the original test in the PR.
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
!
module generic_deferred
  implicit none
  type, abstract :: addable
  contains
    private
    procedure(add), deferred :: a
    generic, public :: operator(+) => a 
  end type addable
  abstract interface
    function add(x, y) result(res)
      import :: addable
      class(addable), intent(in) :: x, y
      class(addable), allocatable :: res
    end function add
  end interface
  type, extends(addable) :: vec
    integer :: i(2)
  contains
    procedure :: a => a_vec
  end type
contains
  function a_vec(x, y) result(res)
    class(vec), intent(in) :: x
    class(addable), intent(in) :: y
    class(addable), allocatable :: res
    integer :: ii(2)
    select type(y)
    class is (vec)
      ii = y%i
    end select 
    allocate(vec :: res)
    select type(res)
    type is (vec)
       res%i = x%i + ii
    end select
  end function
end module generic_deferred
program prog
  use generic_deferred
  implicit none
  type(vec) :: x, y
  class(addable), allocatable :: z
!  x = vec( (/1,2/) );   y = vec( (/2,-2/) )
  x%i = (/1,2/); y%i = (/2,-2/)
  allocate(z, source= x + y)
  select type(z)
  type is(vec)
     if (z%i(1) /= 3 .or. z%i(2) /= 0) then
        write(*,*) 'FAIL'
     else
        write(*,*) 'OK'
     end if
  end select
end program prog
! { dg-final { cleanup-modules "generic_deferred" } }

