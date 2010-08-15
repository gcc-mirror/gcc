! { dg-do compile }
! { dg-options "-std=f2003" }
!
! NULL() actual argument to non-pointer dummies
!

call f(null()) ! { dg-error "Fortran 2008: Null pointer at .1. to non-pointer dummy" }
call g(null()) ! { dg-error "Unexpected NULL.. intrinsic at .1. to dummy" }
call h(null()) ! { dg-error "Unexpected NULL.. intrinsic at .1. to dummy" }
contains
subroutine f(x)
  integer, optional  :: x
end subroutine f
subroutine g(x)
  integer, optional, allocatable  :: x
end subroutine g
subroutine h(x)
  integer :: x
end subroutine h
end
