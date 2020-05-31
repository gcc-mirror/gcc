! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
! PR 94361 - this left open some memory leaks.  Original test case by
! Antony Lewis.

module debug
  private

  Type TypeWithFinal
   contains
     FINAL :: finalizer  !No leak if this line is commented
  end type TypeWithFinal

  Type Tester
     real, dimension(:), allocatable :: Dat
     Type(TypeWithFinal) :: X
  end Type Tester

  Type :: TestType2
     Type(Tester) :: T
  end type TestType2
  public Leaker
contains

  subroutine Leaker
    type(TestType2) :: Test

    allocate(Test%T%Dat(1000))
  end subroutine Leaker

  subroutine finalizer(this)
    Type(TypeWithFinal) :: this
  end subroutine finalizer

end module debug


program run
  use debug
  implicit none
  integer i

  do i=1, 1000
     call Leaker()
  end do

end program run
! { dg-final  { scan-tree-dump-times "__builtin_free\\ \\(ptr2" 2 "original" } }
