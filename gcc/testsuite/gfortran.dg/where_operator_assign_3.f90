! { dg-do compile }
! Tests the fix for PR30407, in which operator assignments did not work
! in WHERE blocks or simple WHERE statements. This tests that the character
! lengths are transmitted OK.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!******************************************************************************
module global
  type :: a
    integer :: b
    character(8):: c
  end type a
  interface assignment(=)
    module procedure a_to_a, c_to_a, a_to_c
  end interface
  interface operator(.ne.)
    module procedure a_ne_a
  end interface

  type(a) :: x(4), y(4)
  logical :: l1(4), t = .true., f= .false.
contains
!******************************************************************************
  elemental subroutine a_to_a (m, n)
    type(a), intent(in) :: n
    type(a), intent(out) :: m
    m%b = len ( trim(n%c))
    m%c = n%c
  end subroutine a_to_a
  elemental subroutine c_to_a (m, n)
    character(8), intent(in) :: n
    type(a), intent(out) :: m
    m%b = m%b + 1
    m%c = n
  end subroutine c_to_a
  elemental subroutine a_to_c (m, n)
    type(a), intent(in) :: n
    character(8), intent(out) :: m
    m = n%c
  end subroutine a_to_c
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
  x = (/a (0, "one"),a (0, "two"),a (0, "three"),a (0, "four")/)
  y = x
  l1 = (/t,f,f,t/)

  call test_where_char1
  call test_where_char2
  if (any(y .ne. &
    (/a(4, "null"), a(8, "non-null"), a(8, "non-null"), a(4, "null")/))) call abort ()
contains
  subroutine test_where_char1   ! Test a WHERE blocks
    where (l1)
      y = a (0, "null")
    elsewhere
      y = x
    end where
  end subroutine test_where_char1
  subroutine test_where_char2   ! Test a WHERE blocks
    where (y%c .ne. "null")
      y = a (99, "non-null")
    endwhere
  end subroutine test_where_char2
end program test 
! { dg-final { cleanup-modules "global" } }

