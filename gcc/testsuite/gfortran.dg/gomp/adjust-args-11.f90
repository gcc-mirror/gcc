! { dg-do compile }

! Check that the OpenMP syntax with commas between clauses is supported.
! A comma after the directive name is introduced in 5.2, which currently is only
! partially supported.

module main
  use iso_c_binding, only: c_ptr
  implicit none

  type :: struct
    integer :: a
    real :: b
  end type

  interface
    integer function f(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c
    end function
    integer function f0(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c
      !$omp  declare variant (f), match (construct={dispatch}) , &
      !$omp&         adjust_args (nothing: a) ,adjust_args (need_device_ptr: b),adjust_args (need_device_ptr: c)
    end function
  end interface

contains
subroutine test
  integer :: a
  type(c_ptr) :: b
  type(c_ptr) :: c(2)
  type(struct) :: s

  !!$omp dispatch, nocontext(.false.), novariants(.false.)   ! Not supported yet
  !$omp dispatch nocontext(.false.), novariants(.false.)
  s%a = f0 (a, b, c)

end subroutine
end module

module other
  use iso_c_binding, only: c_ptr
  implicit none

  interface
    integer function g(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
    end function
    integer function g0(a, b, c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
      !$omp  declare variant (g), match (construct={dispatch}) , &
      !$omp&         adjust_args (nothing: a) ,adjust_args (need_device_ptr: b),adjust_args (need_device_ptr: c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
! { dg-note "Consider using 'need_device_addr' instead" "" { target *-*-* } .-1 }
    end function
  end interface
end module

subroutine foobar
  use iso_c_binding, only: c_ptr
  implicit none

  interface
    integer function h(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
    end function
    integer function h0(a, b, c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
      !$omp  declare variant (h), match (construct={dispatch}) , &
      !$omp&         adjust_args (nothing: a) ,adjust_args (need_device_ptr: b),adjust_args (need_device_ptr: c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
! { dg-note "Consider using 'need_device_addr' instead" "" { target *-*-* } .-1 }
    end function
  end interface
end


subroutine outer
contains
subroutine inner
  use iso_c_binding, only: c_ptr
  implicit none

  interface
    integer function st(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
    end function
    integer function st0(a, b, c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c(:)
      !$omp  declare variant (st), match (construct={dispatch}) , &
      !$omp&         adjust_args (nothing: a) ,adjust_args (need_device_ptr: b),adjust_args (need_device_ptr: c)  ! { dg-error "Argument 'c' at .1. to list item in 'need_device_ptr' at .2. must be a scalar of TYPE\\(C_PTR\\)" }
! { dg-note "Consider using 'need_device_addr' instead" "" { target *-*-* } .-1 }
    end function
  end interface
end subroutine inner
end subroutine outer
