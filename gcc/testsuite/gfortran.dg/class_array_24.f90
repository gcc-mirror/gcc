! { dg-do run }
! { dg-additional-options "-O2 -fdump-tree-optimized" }
!
! PR fortran/123961 - SIZE and class array valued constructors and functions

module test_overload_m
  implicit none

  type :: foo_t
  end type foo_t

  interface foo_t
     module procedure foo_t_0_
     module procedure foo_t_1_
     module procedure foo_c_0_
     module procedure foo_c_1_
  end interface foo_t

contains

  function foo_t_0_(i) result(foo)
    integer, intent(in)      :: i
    type(foo_t), allocatable :: foo
    allocate (foo)
  end function foo_t_0_

  function foo_t_1_(i) result(foo)
    integer, intent(in)      :: i(:)
    type(foo_t), allocatable :: foo(:)

    allocate (foo(size (i)))
  end function foo_t_1_

  function foo_c_0_(r) result(foo)
    real, intent(in)          :: r
    class(foo_t), allocatable :: foo
    allocate (foo)
  end function foo_c_0_

  function foo_c_1_(r) result(foo)
    real, intent(in)          :: r(:)
    class(foo_t), allocatable :: foo(:)

    allocate (foo(size (r)))
  end function foo_c_1_

end module test_overload_m

program test_overload
   use test_overload_m
   implicit none

   if (size (foo_t([1,2,3])) /= 3) stop 1   ! Optimized
   if (size (foo_t([1.,2.])) /= 2) stop 2   ! Optimized

end program test_overload

! { dg-final { scan-tree-dump-not "_gfortran_error_stop_numeric" "optimized" } }
