! { dg-do run }
! Tests dynamic dispatch of class functions, spread over
! different modules. Apart from the location of the derived
! type declarations, this test is the same as
! dynamic_dispatch_1.f03
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
module m1
  type :: t1
    integer :: i = 42
    procedure(make_real), pointer :: ptr
  contains
    procedure, pass :: real => make_real
    procedure, pass :: make_integer
    procedure, pass :: prod => i_m_j
    generic, public :: extract => real, make_integer
  end type t1
contains
  real function make_real (arg)
    class(t1), intent(in) :: arg
    make_real = real (arg%i)
  end function make_real

  integer function make_integer (arg, arg2)
    class(t1), intent(in) :: arg
    integer :: arg2
    make_integer = arg%i * arg2
  end function make_integer

  integer function i_m_j (arg)
    class(t1), intent(in) :: arg
        i_m_j = arg%i
  end function i_m_j
end module m1

module m2
  use m1
  type, extends(t1) :: t2
    integer :: j = 99
  contains
    procedure, pass :: real => make_real2
    procedure, pass :: make_integer => make_integer_2
    procedure, pass :: prod => i_m_j_2
  end type t2
contains
  real function make_real2 (arg)
    class(t2), intent(in) :: arg
    make_real2 = real (arg%j)
  end function make_real2

  integer function make_integer_2 (arg, arg2)
    class(t2), intent(in) :: arg
    integer :: arg2
    make_integer_2 = arg%j * arg2
  end function make_integer_2

  integer function i_m_j_2 (arg)
    class(t2), intent(in) :: arg
        i_m_j_2 = arg%j
  end function i_m_j_2
end module m2

  use m1
  use m2
  type, extends(t1) :: l1
    character(16) :: chr
  end type l1
  class(t1), pointer :: a !=> NULL()
  type(t1), target :: b
  type(t2), target :: c
  type(l1), target :: d
  a => b                                   ! declared type in module m1
  if (a%real() .ne. real (42)) call abort
  if (a%prod() .ne. 42) call abort
  if (a%extract (2) .ne. 84) call abort
  a => c                                   ! extension in module m2
  if (a%real() .ne. real (99)) call abort
  if (a%prod() .ne. 99) call abort
  if (a%extract (3) .ne. 297) call abort
  a => d                                   ! extension in main
  if (a%real() .ne. real (42)) call abort
  if (a%prod() .ne. 42) call abort
  if (a%extract (4) .ne. 168) call abort
end
! { dg-final { cleanup-modules "m1, m2" } }
