! { dg-do compile }
! Tests the fix for PR30407, in which operator assignments did not work
! in WHERE blocks or simple WHERE statements.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!******************************************************************************
module global
  type :: a
    integer :: b
    integer :: c
  end type a
  interface assignment(=)
    module procedure a_to_a
  end interface
  interface operator(.ne.)
    module procedure a_ne_a
  end interface

  type(a) :: x(4), y(4), z(4), u(4, 4)
  logical :: l1(4), t = .true., f= .false.
contains
!******************************************************************************
  elemental subroutine a_to_a (m, n)
    type(a), intent(in) :: n
    type(a), intent(out) :: m
    m%b = n%b + 1
    m%c = n%c
  end subroutine a_to_a
!******************************************************************************
  elemental logical function a_ne_a (m, n)
    type(a), intent(in) :: n
    type(a), intent(in) :: m
    a_ne_a = (m%b .ne. n%b) .or. (m%c .ne. n%c)
  end function a_ne_a
!******************************************************************************
  elemental function foo (m)
    type(a) :: foo
    type(a), intent(in) :: m
    foo%b = 0
    foo%c = m%c
  end function foo  
end module global
!******************************************************************************
program test
  use global
  x = (/a (0, 1),a (0, 2),a (0, 3),a (0, 4)/)
  y = x
  z = x
  l1 = (/t, f, f, t/)

  call test_where_1
  if (any (y .ne. (/a (2, 1),a (2, 2),a (2, 3),a (2, 4)/))) call abort ()

  call test_where_2
  if (any (y .ne. (/a (1, 0),a (2, 2),a (2, 3),a (1, 0)/))) call abort ()
  if (any (z .ne. (/a (3, 4),a (1, 0),a (1, 0),a (3, 1)/))) call abort ()

  call test_where_3
  if (any (y .ne. (/a (1, 0),a (1, 2),a (1, 3),a (1, 0)/))) call abort ()

  y = x
  call test_where_forall_1
  if (any (u(4, :) .ne. (/a (1, 4),a (2, 2),a (2, 3),a (1, 4)/))) call abort ()

  l1 = (/t, f, t, f/)
  call test_where_4
  if (any (x .ne. (/a (1, 1),a (2, 1),a (1, 3),a (2, 3)/))) call abort ()

contains
!******************************************************************************
  subroutine test_where_1        ! Test a simple WHERE
    where (l1) y = x
  end subroutine test_where_1
!******************************************************************************
  subroutine test_where_2        ! Test a WHERE blocks
    where (l1)
      y = a (0, 0)
      z = z(4:1:-1)
    elsewhere
      y = x
      z = a (0, 0)
    end where
  end subroutine test_where_2
!******************************************************************************
  subroutine test_where_3        ! Test a simple WHERE with a function assignment
    where (.not. l1) y = foo (x)
  end subroutine test_where_3
!******************************************************************************
  subroutine test_where_forall_1 ! Test a WHERE in a FORALL block
    forall (i = 1:4)
      where (.not. l1)
        u(i, :) = x
      elsewhere
        u(i, :) = a(0, i)
      endwhere
    end forall
  end subroutine test_where_forall_1
!******************************************************************************
  subroutine test_where_4       ! Test a WHERE assignment with dependencies
    where (l1(1:3))
      x(2:4) = x(1:3)
    endwhere
  end subroutine test_where_4
end program test 
! { dg-final { cleanup-modules "global" } }

