! { dg-do compile }
!
! PR 60507: Passing function call into procedure argument not caught
!
! Contributed by Vladimir Fuka <vladimir.fuka@gmail.com>

type :: t
  procedure(g), pointer, nopass :: ppc
end type

procedure(g), pointer :: pp
type(t)::x

print *, f(g)
print *, f(g())      ! { dg-error "Expected a procedure for argument" }
pp => g
print *, f(pp)
print *, f(pp())     ! { dg-error "Expected a procedure for argument" }
x%ppc => g
print *, f(x%ppc)
print *, f(x%ppc())  ! { dg-error "Expected a procedure for argument" }

contains

  real function f(fun)
    procedure(g) :: fun
    f = fun()
  end function

  real function g()
    g = 1.
  end function

end
