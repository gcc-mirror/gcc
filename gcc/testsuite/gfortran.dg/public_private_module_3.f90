! { dg-do link }
! { dg-additional-sources public_private_module_4.f90 }
!
! PR fortran/52916
! Cf. PR fortran/40973
!
! Ensure that PRIVATE specific functions do not get
! marked as TREE_PUBLIC() = 0, if the generic name is
! PUBLIC.
!
module m
  interface gen
    module procedure bar
  end interface gen

  type t
  end type t

  interface operator(.myop.)
    module procedure my_op
  end interface

  interface operator(+)
    module procedure my_plus
  end interface

  interface assignment(=)
    module procedure my_assign
  end interface

  private :: bar, my_op, my_plus, my_assign
contains
  subroutine bar()
    print *, "bar"
  end subroutine bar
  function my_op(op1, op2) result(res)
    type(t) :: res
    type(t), intent(in) :: op1, op2
  end function my_op
  function my_plus(op1, op2) result(res)
    type(t) :: res
    type(t), intent(in) :: op1, op2
  end function my_plus
  subroutine my_assign(lhs, rhs)
    type(t), intent(out) :: lhs
    type(t), intent(in) :: rhs
  end subroutine my_assign
end module m

module m2
  type t2
  contains
    procedure, nopass :: func => foo
  end type t2
  private :: foo
contains
  subroutine foo()
  end subroutine foo
end module m2
