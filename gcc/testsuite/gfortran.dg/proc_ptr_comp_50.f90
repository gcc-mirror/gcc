! { dg-do compile }
!
! PR 70601: [5/6/7 Regression] [OOP] ICE on procedure pointer component call
!
! Contributed by zmi <zmi007@gmail.com>

program test
  implicit none

  type :: concrete_type
    procedure (run_concrete_type), pointer :: run
  end type

  type(concrete_type), allocatable :: concrete

  allocate(concrete)
  concrete % run => run_concrete_type
  call concrete % run()

contains

   subroutine run_concrete_type(this)
      class(concrete_type) :: this
   end subroutine

end
