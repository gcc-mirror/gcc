! { dg-do run }
! Test the fix for PR113885, where not only was there a gimplifier ICE
! for a derived type 't' with no components but this version gave wrong
! results.
! Contributed by David Binderman  <dcb314@hotmail.com>
!
module types
  type t
     integer :: i
   contains
     final :: finalize
  end type t
  integer :: ctr = 0
contains
  impure elemental subroutine finalize(x)
    type(t), intent(inout) :: x
    ctr = ctr + 1
  end subroutine finalize
end module types

impure elemental function elem(x)
  use types
  type(t), intent(in) :: x
  type(t) :: elem
  elem%i = x%i + 1
end function elem

impure elemental function elem2(x, y)
  use types
  type(t), intent(in) :: x, y
  type(t) :: elem2
  elem2%i = x%i + y%i
end function elem2

subroutine test1(x)
  use types
  interface
     impure elemental function elem(x)
       use types
       type(t), intent(in) :: x
       type(t) :: elem
     end function elem
  end interface
  type(t) :: x(:)
  type(t), allocatable :: y(:)
  y = x
  x = elem(y)
end subroutine test1

subroutine test2(x)
  use types
  interface
     impure elemental function elem(x)
       use types
       type(t), intent(in) :: x
       type(t) :: elem
     end function elem
     impure elemental function elem2(x, y)
       use types
       type(t), intent(in) :: x, y
       type(t) :: elem2
     end function elem2
  end interface
  type(t) :: x(:)
  type(t), allocatable :: y(:)
  y = x
  x = elem2(elem(y), elem(y))
end subroutine test2

program test113885
  use types
  interface
    subroutine test1(x)
      use types
      type(t) :: x(:)
    end subroutine
    subroutine test2(x)
      use types
      type(t) :: x(:)
    end subroutine
  end interface
  type(t) :: x(2) = [t(1),t(2)]
  call test1 (x)
  if (any (x%i .ne. [2,3])) stop 1
  if (ctr .ne. 6) stop 2
  call test2 (x)
  if (any (x%i .ne. [6,8])) stop 3
  if (ctr .ne. 16) stop 4
end
