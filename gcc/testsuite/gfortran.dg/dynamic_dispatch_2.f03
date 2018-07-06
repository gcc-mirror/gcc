! { dg-do run }
! Tests dynamic dispatch of class subroutines.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
module m
  type :: t1
    integer :: i = 42
    procedure(make_real), pointer :: ptr
  contains
    procedure, pass :: real => make_real
    procedure, pass :: make_integer
    procedure, pass :: prod => i_m_j
    generic, public :: extract => real, make_integer
  end type t1

  type, extends(t1) :: t2
    integer :: j = 99
  contains
    procedure, pass :: real => make_real2
    procedure, pass :: make_integer => make_integer_2
    procedure, pass :: prod => i_m_j_2
  end type t2
contains
  subroutine make_real (arg, arg2)
    class(t1), intent(in) :: arg
    real :: arg2
    arg2 = real (arg%i)
  end subroutine make_real

  subroutine make_real2 (arg, arg2)
    class(t2), intent(in) :: arg
    real :: arg2
    arg2 = real (arg%j)
  end subroutine make_real2

  subroutine make_integer (arg, arg2, arg3)
    class(t1), intent(in) :: arg
    integer :: arg2, arg3
    arg3 = arg%i * arg2
  end subroutine make_integer

  subroutine make_integer_2 (arg, arg2, arg3)
    class(t2), intent(in) :: arg
    integer :: arg2, arg3
    arg3 = arg%j * arg2
  end subroutine make_integer_2

  subroutine i_m_j (arg, arg2)
    class(t1), intent(in) :: arg
    integer :: arg2
        arg2 = arg%i
  end subroutine i_m_j

  subroutine i_m_j_2 (arg, arg2)
    class(t2), intent(in) :: arg
    integer :: arg2
        arg2 = arg%j
  end subroutine i_m_j_2
end module m

  use m
  type, extends(t1) :: l1
    character(16) :: chr
  end type l1
  class(t1), pointer :: a !=> NULL()
  type(t1), target :: b
  type(t2), target :: c
  type(l1), target :: d
  real :: r
  integer :: i

  a => b                                   ! declared type
  call a%real(r)
  if (r .ne. real (42)) STOP 1
  call a%prod(i)
  if (i .ne. 42) STOP 2
  call a%extract (2, i)
  if (i .ne. 84) STOP 3

  a => c                                   ! extension in module
  call a%real(r)
  if (r .ne. real (99)) STOP 4
  call a%prod(i)
  if (i .ne. 99) STOP 5
  call a%extract (3, i)
  if (i .ne. 297) STOP 6

  a => d                                   ! extension in main
  call a%real(r)
  if (r .ne. real (42)) STOP 7
  call a%prod(i)
  if (i .ne. 42) STOP 8
  call a%extract (4, i)
  if (i .ne. 168) STOP 9
end
