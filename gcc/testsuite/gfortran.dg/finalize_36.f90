! { dg-do run }
! { dg-additional-options "-fdump-tree-original" }
! PR 94109
! This used to leak memory.  Test case by Antony Lewis.
    module debug
    implicit none

    Type Tester
        real, dimension(:), allocatable :: Dat, Dat2
    end Type

    Type TestType2
        Type(Tester) :: T
    end type TestType2

    contains

    subroutine Leaker
    class(TestType2), pointer :: ActiveState
    Type(Tester) :: Temp

    allocate(Temp%Dat2(10000))

    allocate(TestType2::ActiveState)
    ActiveState%T = Temp
    deallocate(ActiveState)

    end subroutine

    end module


    program run
    use debug

    call Leaker()

    end program
! { dg-final { scan-tree-dump-times "__builtin_free\\ \\(ptr2" 4 "original" } }
