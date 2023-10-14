! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
!  Test the fix for PR86481
!
! Contributed by Rich Townsend  <townsend@astro.wisc.edu>
!
program simple_leak

  implicit none

  type, abstract :: foo_t
  end type foo_t

  type, extends(foo_t) :: foo_a_t
     real(8), allocatable :: a(:)
  end type foo_a_t

  type, extends(foo_t) ::  bar_t
     class(foo_t), allocatable :: f
  end type bar_t

  integer, parameter :: N = 2
  integer, parameter :: D = 3

  type(bar_t) :: b(N)
  integer     :: i

  do i = 1, N
     b(i) = func_bar(D)
  end do

  do i = 1, N
     deallocate (b(i)%f)
  end do

contains

  function func_bar (D) result (b)

    integer, intent(in) :: D
    type(bar_t)         :: b

    allocate(b%f, SOURCE=func_foo(D))

  end function func_bar

  !****

  function func_foo (D) result (f)

    integer, intent(in)       :: D
    class(foo_t), allocatable :: f

    allocate(f, SOURCE=func_foo_a(D)) ! Lose one of these for each allocation

  end function func_foo

  !****

  function func_foo_a (D) result (f)

    integer, intent(in) :: D
    type(foo_a_t)       :: f

    allocate(f%a(D))  ! Lose one of these for each allocation => N*D*elem_size(f%a)

  end function func_foo_a

end program simple_leak
! { dg-final { scan-tree-dump-times "\>_final" 6 "original" } }
