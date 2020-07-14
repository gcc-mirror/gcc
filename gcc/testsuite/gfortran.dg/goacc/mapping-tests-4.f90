subroutine foo
  type one
    integer i, j
  end type
  type two
    type(one) A, B
  end type

  type(two) x

! This is accepted at present, although it represents a probably-unintentional
! overlapping subcopy.
  !$acc enter data copyin(x%A, x%A%i)
! But this raises an error.
  !$acc enter data copyin(x%A, x%A%i, x%A%i)
! { dg-error ".x.a.i. appears more than once in map clauses" "" { target *-*-* } .-1 }
end
