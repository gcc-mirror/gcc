! { dg-do compile }
!
! PR fortran/47448
!
! ASSIGNMENT(=) checks. Defined assignment is allowed if and only if
! it does not override an intrinsic assignment.
!

module test1
  interface assignment(=)
     module procedure valid, valid2
  end interface
contains
  ! Valid: scalar = array
  subroutine valid (lhs,rhs)
    integer, intent(out) ::  lhs
    integer, intent(in) :: rhs(:)
    lhs = rhs(1) 
  end subroutine valid

  ! Valid: array of different ranks
  subroutine valid2 (lhs,rhs)
    integer, intent(out) ::  lhs(:)
    integer, intent(in) :: rhs(:,:)
    lhs(:) = rhs(:,1) 
  end subroutine valid2
end module test1

module test2
  interface assignment(=)
     module procedure invalid
  end interface
contains
  ! Invalid: scalar = scalar
  subroutine invalid (lhs,rhs) ! { dg-error "must not redefine an INTRINSIC type assignment" }
    integer, intent(out) ::  lhs
    integer, intent(in) :: rhs
    lhs = rhs
  end subroutine invalid
end module test2

module test3
  interface assignment(=)
     module procedure invalid2
  end interface
contains
  ! Invalid: array = scalar
  subroutine invalid2 (lhs,rhs) ! { dg-error "must not redefine an INTRINSIC type assignment" }
    integer, intent(out) ::  lhs(:)
    integer, intent(in) :: rhs
    lhs(:) = rhs
  end subroutine invalid2
end module test3

module test4
  interface assignment(=)
     module procedure invalid3
  end interface
contains
  ! Invalid: array = array for same rank
  subroutine invalid3 (lhs,rhs) ! { dg-error "must not redefine an INTRINSIC type assignment" }
    integer, intent(out) ::  lhs(:)
    integer, intent(in) :: rhs(:)
    lhs(:) = rhs(:)
  end subroutine invalid3
end module test4
