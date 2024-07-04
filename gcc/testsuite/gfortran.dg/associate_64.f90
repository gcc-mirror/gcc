! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Tests the fix for PR89645 and 99065, in which derived type or class functions,
! used as associate selectors and which were parsed after the containing scope
! of the associate statement, caused "no IMPLICIT type" and "Syntax" errors.
!
! Contributed by Ian Harvey  <ian_harvey@bigpond.com>
!
module m
  implicit none
  type t
    integer :: i = 0
  end type t
  integer :: i = 0
  type(t), parameter :: test_array (2) = [t(42),t(84)], &
                        test_scalar = t(99)
end module m

! DERIVED TYPE VERSION OF THE PROBLEM, AS REPORTED IN THE PRs
module type_selectors
  use m
  implicit none
  private
  public foo1
contains
! Since these functions are parsed first, the symbols are available for
! parsing in 'foo'.
  function bar1() result(res) ! The array version caused syntax errors in foo
    type(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end
  function bar2() result(res) ! Scalar class functions were OK - test anyway
    type(t), allocatable :: res
    allocate (res, source = test_scalar)
  end
  subroutine foo1()
! First the array selector
    associate (var1 => bar1())
      if (any (var1%i .ne. test_array%i)) stop 1
      if (var1(2)%i .ne. test_array(2)%i) stop 2
    end associate
! Now the scalar selector
    associate (var2 => bar2())
      if (var2%i .ne. test_scalar%i) stop 3
    end associate

! Now the array selector that needed fixing up because the function follows....
    associate (var1 => bar3())
      if (any (var1%i .ne. test_array%i)) stop 4
      if (var1(2)%i .ne. test_array(2)%i) stop 5
    end associate
! ....and equivalent scalar selector
    associate (var2 => bar4())
      if (var2%i .ne. test_scalar%i) stop 6
    end associate
  end subroutine foo1

! These functions are parsed after 'foo' so the symbols were not available
! for the selectors and the fixup, tested here, was necessary.
  function bar3() result(res)
    class(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end

  function bar4() result(res)
    class(t), allocatable :: res
    allocate (res, source = t(99))
  end
end module type_selectors

! CLASS VERSION OF THE PROBLEM, WHICH REQUIRED MOST OF THE WORK!
module class_selectors
  use m
  implicit none
  private
  public foo2
contains

! Since these functions are parsed first, the symbols are available for
! parsing in 'foo'.
  function bar1() result(res) ! The array version caused syntax errors in foo
    class(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end

  function bar2() result(res) ! Scalar class functions were OK - test anyway
    class(t), allocatable :: res
    allocate (res, source = t(99))
  end

  subroutine foo2()
! First the array selector
    associate (var1 => bar1())
      if (any (var1%i .ne. test_array%i)) stop 7
      if (var1(2)%i .ne. test_array(2)%i) stop 8
      select type (x => var1)
        type is (t)
          if (any (x%i .ne. test_array%i)) stop 9
          if (x(1)%i .ne. test_array(1)%i) stop 10
        class default
          stop 11
      end select
    end associate

! Now scalar selector
    associate (var2 => bar2())
      select type (z => var2)
        type is (t)
          if (z%i .ne. test_scalar%i) stop 12
        class default
          stop 13
      end select
    end associate

! This is the array selector that needed the fixup.
    associate (var1 => bar3())
      if (any (var1%i .ne. test_array%i)) stop 14
      if (var1(2)%i .ne. test_array(2)%i) stop 15
      select type (x => var1)
        type is (t)
          if (any (x%i .ne. test_array%i)) stop 16
          if (x(1)%i .ne. test_array(1)%i) stop 17
        class default
          stop 18
      end select
    end associate

! Now the equivalent scalar selector
    associate (var2 => bar4())
      select type (z => var2)
        type is (t)
          if (z%i .ne. test_scalar%i) stop 19
        class default
          stop 20
      end select
    end associate

  end subroutine foo2

! These functions are parsed after 'foo' so the symbols were not available
! for the selectors and the fixup, tested here, was necessary.
  function bar3() result(res)
    class(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end

  function bar4() result(res)
    class(t), allocatable :: res
    allocate (res, source = t(99))
  end
end module class_selectors

! THESE TESTS CAUSED PROBLEMS DURING DEVELOPMENT FOR BOTH PARSING ORDERS.
module problem_selectors
  implicit none
  private
  public foo3, foo4
  type t
    integer :: i
  end type t
  type s
    integer :: i
    type(t) :: dt
  end type s
  type(t), parameter :: test_array (2) = [t(42),t(84)], &
                        test_scalar = t(99)
  type(s), parameter :: test_sarray (2) = [s(142,t(42)),s(184,t(84))]
contains

  subroutine foo3()
    integer :: i
    block
      associate (var1 => bar7())
        if (any (var1%i .ne. test_array%i)) stop 21
        if (var1(2)%i .ne. test_array(2)%i) stop 22
        associate (z => var1(1)%i)
           if (z .ne. 42) stop 23
        end associate
    end associate
    end block

    associate (var2 => bar8())
      i = var2(2)%i
      associate (var3 => var2%dt)
        if (any (var3%i .ne. test_sarray%dt%i)) stop 24
      end associate
      associate (var4 => var2(2))
        if (var4%i .ne. 184) stop 25
      end associate
    end associate
  end subroutine foo3

  function bar7() result(res)
    type(t), allocatable :: res(:)
    allocate (res, source = test_array)
  end

  function bar8() result(res)
    type(s), allocatable :: res(:)
    allocate (res, source = test_sarray)
  end

  subroutine foo4()
    integer :: i
    block
      associate (var1 => bar7())
        if (any (var1%i .ne. test_array%i)) stop 26
        if (var1(2)%i .ne. test_array(2)%i) stop 27
        associate (z => var1(1)%i)
           if (z .ne. 42) stop 28
        end associate
    end associate
    end block

    associate (var2 => bar8())
      i = var2(2)%i
      associate (var3 => var2%dt)
        if (any (var3%i .ne. test_sarray%dt%i)) stop 29
      end associate
      associate (var4 => var2(2))
        if (var4%i .ne. 184) stop 30
      end associate
    end associate
  end subroutine foo4

end module problem_selectors

module more_problem_selectors
  implicit none
  private
  public foo5, foo6
  type t
    integer :: i = 0
  end type t
  type s
    integer :: i = 0
    type(t) :: dt
  end type s
contains
! In this version, the order of declarations of 't' and 's' is such that
! parsing var%i sets the type of var to 't' and this is corrected to 's'
! on parsing var%dt%i
  subroutine foo5()
    associate (var3 => bar3())
      if (var3%i .ne. 42) stop 31
      if (var3%dt%i .ne. 84) stop 32
    end associate

! Repeat with class version
    associate (var4 => bar4())
      if (var4%i .ne. 84) stop 33
      if (var4%dt%i .ne. 168) stop 34
      select type (x => var4)
        type is (s)
          if (x%i .ne. var4%i) stop 35
          if (x%dt%i .ne. var4%dt%i) stop 36
        class default
          stop 37
      end select
    end associate

! Ditto with no type component clues for select type
    associate (var5 => bar4())
      select type (z => var5)
        type is (s)
          if (z%i .ne. 84) stop 38
          if (z%dt%i .ne. 168) stop 39
        class default
          stop 40
      end select
    end associate
  end subroutine foo5

! Now the array versions
  subroutine foo6()
    class(s), allocatable :: elem
    associate (var6 => bar5())
      if (var6(1)%i .ne. 42) stop 41
      if (any (var6%dt%i .ne. [84])) stop 42
    end associate

! Class version with an assignment to a named variable
    associate (var7 => bar6())
      elem = var7(2)
      if (any (var7%i .ne. [84, 168])) stop 43
      if (any (var7%dt%i .ne. [168, 336])) stop 44
    end associate
    if (elem%i .ne. 168) stop 45
    if (elem%dt%i .ne. 336) stop 46

    select type (z => elem)
      type is (s)
        if (z%i .ne. 168) stop 47
        if (z%dt%i .ne. 336) stop 48
      class default
        stop 49
    end select

! Array version without type clues before select type
    associate (var8 => bar6())
      select type (z => var8)
        type is (s)
          if (any (z%i .ne. [84,168])) stop 50
          if (any (z%dt%i .ne. [168,336])) stop 51
        class default
          stop 52
      end select
    end associate
  end subroutine foo6

  type(s) function bar3()
    bar3= s(42, t(84))
  end

  function bar4() result(res)
    class(s), allocatable :: res
    res = s(84, t(168))
  end

  function bar5() result (res)
    type(s), allocatable :: res(:)
    res = [s(42, t(84))]
  end

  function bar6() result (res)
    class(s), allocatable :: res(:)
    res = [s(84, t(168)),s(168, t(336))]
  end

end module more_problem_selectors

program test
  use type_selectors
  use class_selectors
  use problem_selectors
  use more_problem_selectors
  call foo1()
  call foo2()
  call foo3()
  call foo4()
  call foo5()
  call foo6()
end program test
! { dg-final { scan-tree-dump-times "__builtin_free" 18 "original" } }
