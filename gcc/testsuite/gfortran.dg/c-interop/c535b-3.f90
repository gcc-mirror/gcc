! PR 101334
! { dg-do compile }
! { dg-additional-options "-fcoarray=single" }
!
! TS 29113
! C535b An assumed-rank variable name shall not appear in a designator
! or expression except as an actual argument corresponding to a dummy
! argument that is assumed-rank, the argument of the C_LOC function
! in the ISO_C_BINDING intrinsic module, or the first argument in a
! reference to an intrinsic inquiry function.
!
! This has been renamed C838 in the Fortran 2018 standard, with C_SIZEOF
! and SELECT_RANK additionally added.
!
! This tests various forms of the 2-argument associated intrinsic.

function test_associated2 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, target :: b(..)
  logical :: test_associated2

  test_associated2 = associated (a, b) ! { dg-error "Assumed.rank" }
end function

function test_associated3 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, target :: b
  logical :: test_associated3

  test_associated3 = associated (a, b) ! { dg-bogus "must be of rank -1" "pr101334" }
end function

function test_associated4 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, target :: b(:)
  logical :: test_associated4

  test_associated4 = associated (a, b) ! { dg-bogus "must be of rank -1" "pr101334" }
end function

function test_associated5 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, target :: b(20)
  logical :: test_associated5

  test_associated5 = associated (a, b) ! { dg-bogus "must be of rank -1" "pr101334" }
end function

function test_associated6 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, pointer :: b(..)
  logical :: test_associated6

  test_associated6 = associated (a, b) ! { dg-error "Assumed.rank" }
end function

function test_associated7 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, pointer :: b
  logical :: test_associated7

  test_associated7 = associated (a, b) ! { dg-bogus "must be of rank -1" "pr101334" }
end function

function test_associated8 (a, b)
  implicit none
  integer, pointer :: a(..)
  integer, pointer :: b(:)
  logical :: test_associated8

  test_associated8 = associated (a, b) ! { dg-bogus "must be of rank -1" "pr101334" }
end function

