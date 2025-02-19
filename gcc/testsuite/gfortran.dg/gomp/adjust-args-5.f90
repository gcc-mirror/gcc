! { dg-do compile }
! { dg-additional-options "-fdump-tree-gimple" }

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
      !$omp  declare variant (f) match (construct={dispatch}) &
      !$omp&         adjust_args (nothing: a) adjust_args (need_device_ptr: b, c)
    end function
    integer function f1(a, b, c)
      import c_ptr
      integer, intent(in) :: a
      type(c_ptr), intent(inout) :: b
      type(c_ptr), intent(out) :: c
      !$omp declare variant (f) match (construct={dispatch}) &
      !$omp&        adjust_args (nothing: a) adjust_args (need_device_ptr: b) adjust_args (need_device_ptr: c)
    end function
  end interface

contains
subroutine test
  integer :: a
  type(c_ptr) :: b
  type(c_ptr) :: c(2)
  type(struct) :: s

  s%a = f0 (a, b, c)
  !$omp dispatch
  s%a = f0 (a, b, c)

  s%b = f1 (a, b, c)
  !$omp dispatch
  s%b = f1 (a, b, c)

end subroutine
end module

! { dg-final { scan-tree-dump-times "__builtin_omp_get_default_device \\(\\);" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(c, D\.\[0-9]+\\);" 2 "gimple" } }
! { dg-final { scan-tree-dump-times "D\.\[0-9]+ = __builtin_omp_get_mapped_ptr \\(b\.\[0-9]+, D\.\[0-9]+\\);" 2 "gimple" } }
