! { dg-do compile }

subroutine foo
  type t
    integer :: i, j
  end type t

  type(t) x

  ! We should reject the duplicate reference here.
!$acc enter data copyin(x%i, x%i)
! { dg-error ".x.i. appears more than once in map clauses" "" { target "*-*-*" } 11 }


end
