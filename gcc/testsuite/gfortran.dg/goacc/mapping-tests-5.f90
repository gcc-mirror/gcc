subroutine foo
  type one
    integer, dimension(10) :: i, j
  end type
  type two
    type(one) A, B
  end type

  type(two) x

  !$acc enter data copyin(x%A%i(5), x%A%i(4), x%A)
! { dg-error ".x.a.i. appears more than once in map clauses" "" { target *-*-* } .-1 }
  !$acc enter data copyin(x%A, x%A%i(5), x%A%i(4))
! { dg-error ".x.a.i. appears more than once in map clauses" "" { target *-*-* } .-1 }
end
