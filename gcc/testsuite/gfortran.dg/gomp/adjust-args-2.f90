! Test resolution of OMP clause adjust_args
! { dg-do compile } 

module main
  implicit none
interface
subroutine f1 (i)
  integer, intent(inout) :: i
end subroutine
end interface
contains

  subroutine f3 (i)
    integer, intent(inout) :: i
    !$omp declare variant (f1) match (construct={dispatch}) adjust_args (nothing: z)  ! { dg-error "Symbol 'z' at .1. has no IMPLICIT type" }
! { dg-error "Expected dummy parameter name or a positive integer at .1." "" { target *-*-* } .-1 }
  end subroutine
  
end module
