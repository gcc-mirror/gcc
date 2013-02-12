! { dg-do compile }
!
! PR fortran/54107
! Recursive interfaces used to lead to an infinite recursion during
! translation.

module m
 contains
  subroutine foo (arg) 
    procedure(foo) :: arg 
  end subroutine 
  function foo2 (arg) result(r)
    procedure(foo2) :: arg
    procedure(foo2), pointer :: r
  end function 
  subroutine bar (arg) 
    procedure(baz) :: arg 
  end subroutine 
  subroutine baz (arg) 
    procedure(bar) :: arg 
  end subroutine 
end module m
