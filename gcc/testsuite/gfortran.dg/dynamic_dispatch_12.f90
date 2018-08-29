! { dg-do run }
!
! PR 59654: [4.8/4.9 Regression] [OOP] Broken function table with complex OO use case
!
! Contributed by Thomas Clune <Thomas.L.Clune@nasa.gov>

module TestResult_mod
  implicit none

  type TestResult
    integer :: numRun = 0
  contains
    procedure :: run
    procedure, nopass :: getNumRun
  end type

contains

  subroutine run (this)
    class (TestResult) :: this
    this%numRun = this%numRun + 1
  end subroutine

  subroutine getNumRun()
   end subroutine

end module


module BaseTestRunner_mod
  implicit none

  type :: BaseTestRunner
  contains
    procedure, nopass :: norun
  end type

contains

  function norun () result(result)
    use TestResult_mod, only: TestResult
    type (TestResult) :: result
  end function

end module


module TestRunner_mod
  use BaseTestRunner_mod, only: BaseTestRunner
  implicit none
end module


program main
  use TestRunner_mod, only: BaseTestRunner
  use TestResult_mod, only: TestResult
  implicit none

  type (TestResult) :: result

  call runtest (result)
  
contains

  subroutine runtest (result)
    use TestResult_mod, only: TestResult
    class (TestResult) :: result
    call result%run()
    if (result%numRun /= 1) STOP 1
  end subroutine

end
