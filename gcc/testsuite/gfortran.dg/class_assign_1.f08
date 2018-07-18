! { dg-do run }
!
! Check that reallocation of the lhs is done with the correct memory size.


module base_mod

  type, abstract :: base
  contains
    procedure(base_add), deferred :: add
    generic :: operator(+) => add
  end type base

  abstract interface
    module function base_add(l, r) result(res)
      class(base), intent(in) :: l
      integer, intent(in) :: r
      class(base), allocatable :: res
    end function base_add
  end interface

contains

  subroutine foo(x)
    class(base), intent(inout), allocatable :: x
    class(base), allocatable :: t

    t = x + 2
    x = t + 40
  end subroutine foo

end module base_mod

module extend_mod
  use base_mod

  type, extends(base) :: extend
    integer :: i
  contains
    procedure :: add
  end type extend

contains
  module function add(l, r) result(res)
    class(extend), intent(in) :: l
    integer, intent(in) :: r
    class(base), allocatable :: res
    select type (l)
      class is (extend)
        res = extend(l%i + r)
      class default
        error stop "Unkown class to add to."
    end select
  end function
end module extend_mod

program test_poly_ass
  use extend_mod
  use base_mod

  class(base), allocatable :: obj
  obj = extend(0)
  call foo(obj)
  select type (obj)
    class is (extend)
      if (obj%i /= 42) error stop
    class default
      error stop "Result's type wrong."
  end select
end program test_poly_ass

