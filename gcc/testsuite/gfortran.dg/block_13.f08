! { dg-do run }
! Checks the fix for PR57959. The first assignment to a was proceeding
! without a deep copy. Since the anum field of 'uKnot' was being pointed
! to twice, the frees in the finally block, following the BLOCK caused
! a double free.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program main
  implicit none
  type :: type1
    real, allocatable :: anum
    character(len = :), allocatable :: chr
  end type type1
  real, parameter :: five = 5.0
  real, parameter :: point_one = 0.1

  type :: type2
    type(type1) :: temp
  end type type2
  block
    type(type1) :: uKnot
    type(type2) :: a

    uKnot = type1 (five, "hello")
    call check (uKnot%anum, five)
    call check_chr (uKnot%chr, "hello")

    a = type2 (uKnot) ! Deep copy needed here
    call check (a%temp%anum, five)
    call check_chr (a%temp%chr, "hello")

    a = type2 (type1(point_one, "goodbye")) ! Not here
    call check (a%temp%anum, point_one)
    call check_chr (a%temp%chr, "goodbye")

    a = type2 (foo (five)) ! Not here
    call check (a%temp%anum, five)
    call check_chr (a%temp%chr, "foo set me")
  end block
contains
  subroutine check (arg1, arg2)
    real :: arg1, arg2
    if (arg1 .ne. arg2) call abort ()
  end subroutine

  subroutine check_chr (arg1, arg2)
    character(*) :: arg1, arg2
    if (len (arg1) .ne. len (arg2)) call abort
    if (arg1 .ne. arg2) call abort
  end subroutine

  type(type1) function foo (arg)
    real :: arg
    foo = type1 (arg, "foo set me")
  end function
end

