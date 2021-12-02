! { dg-do compile }
! PR fortran/103392 - ICE in simplify_bound

program p
  integer, allocatable :: a(1:1) ! { dg-error "deferred shape or assumed rank" }
  integer :: b(1) = lbound(a)    ! { dg-error "does not reduce" }
  integer :: c(1) = ubound(a)    ! { dg-error "does not reduce" }
end

subroutine s(x, y)
  type t
     integer :: i(3)
  end type t
  type(t), pointer     :: x(:)
  type(t), allocatable :: y(:)
  integer, parameter   :: m(1) = ubound (x(1)% i)
  integer              :: n(1) = ubound (y(1)% i)
end subroutine s
