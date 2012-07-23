! { dg-do compile }
! { dg-options "-std=f2008ts" }
!
! PR fortran/48820
!
! Assumed-rank constraint checks and other diagnostics
!

subroutine valid1a(x)
  integer, intent(in), pointer, contiguous :: x(..)
end subroutine valid1a

subroutine valid1(x)
  integer, intent(in) :: x(..)
end subroutine valid1

subroutine valid2(x)
 type(*) :: x
end subroutine valid2

subroutine foo99(x)
  integer  x(99)
  call valid1(x) ! { dg-error "Procedure 'valid1' at .1. with assumed-rank dummy argument 'x' must have an explicit interface" }
  call valid2(x(1)) ! { dg-error "Procedure 'valid2' at .1. with assumed-type dummy argument 'x' must have an explicit interface" }
end subroutine foo99

subroutine foo(x)
  integer :: x(..)
  print *, ubound(x,dim=2000) ! { dg-error "is not a valid dimension index" }
  call bar(x) ! { dg-error "Assumed-rank argument requires an explicit interface" }
  call intnl(x) ! { dg-error "requires that the dummy argument 'x' has assumed-rank" }
contains
  subroutine intnl(x)
    integer :: x(:)
  end subroutine intnl
end subroutine foo

subroutine foo2(x)
  integer :: x(..)
  call valid3(x(:)) ! { dg-error "Assumed-rank variable x at .1. shall not have a subobject reference" }
  call valid3(x+1)  ! { dg-error "Assumed-rank variable x at .1. may only be used as actual argument" }
contains
  subroutine valid3(y)
    integer :: y(..)
  end subroutine
end subroutine

subroutine foo3()
  integer :: x(..) ! { dg-error "Assumed-rank array at .1. must be a dummy argument" }
end subroutine
