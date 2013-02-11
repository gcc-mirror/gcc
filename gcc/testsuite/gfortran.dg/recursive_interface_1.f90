! { dg-do compile }
!
! PR fortran/54107
! The compiler used to ICE on recursive interfaces.

module m
 contains
  function foo() result(r1)
    procedure(foo), pointer :: r1 
  end function foo

  function bar() result(r2)
    procedure(baz), pointer :: r2
  end function bar

  function baz() result(r3)
    procedure(bar), pointer :: r3
  end function baz
end module m

