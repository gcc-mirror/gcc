! { dg-do compile }

module implicit_2
  ! This should cause an error if function types are resolved from the
  ! module namespace.
  implicit none
  type t
    integer i
  end type
contains
! This caused an ICE because we were trying to apply the implicit type
! after we had applied the explicit type.
subroutine test()
  implicit type (t) (v)
  type (t) v1, v2
  v1%i = 1
  call foo (v2%i)
end subroutine

! A similar error because we failed to apply the implicit type to a function.
! This is a contained function to check we lookup the type in the function
! namespace, not it's parent.
function f() result (val)
  implicit type (t) (v)

  val%i = 1
end function

! And again for a result variable.
function fun()
  implicit type (t) (f)

  fun%i = 1
end function

! intrinsic types are resolved later than derived type, so check those as well.
function test2()
  implicit integer (t)
  test2 = 42
end function
subroutine bar()
  ! Check that implicit types are applied to names already known to be
  ! variables.
  implicit type(t) (v)
  save v
  v%i = 42
end subroutine
end module
