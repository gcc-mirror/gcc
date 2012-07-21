! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/48820
!
! Assumed-rank constraint checks and other diagnostics
!

subroutine foo(x) ! { dg-error "Assumed-type variable x at .1. may not have the INTENT.OUT. attribute" }
  type(*), intent(out) :: x
end subroutine

subroutine bar(x)
  integer, intent(out) :: x(..)
end subroutine bar

subroutine foo3(y)
  integer :: y(..)
  y = 7           ! { dg-error "Assumed-rank variable y at .1. may only be used as actual argument" }
  print *, y + 10 ! { dg-error "Assumed-rank variable y at .1. may only be used as actual argument" }
  print *, y      ! { dg-error "Assumed-rank variable y at .1. may only be used as actual argument" }
end subroutine

subroutine foo2(x, y)
  integer :: x(..), y(..)
  call valid3(x(:)) ! { dg-error "Assumed-rank variable x at .1. shall not have a subobject reference" }
contains
  subroutine valid3(y)
    integer :: y(..)
  end subroutine
end subroutine

subroutine foo4(x)
  integer, codimension[*] :: x(..) ! { dg-error "The assumed-rank array at .1. shall not have a codimension" }
end subroutine

subroutine foo5(y) ! { dg-error "may not have the VALUE or CODIMENSION attribute" }
  integer :: y(..)[*]
end subroutine
