! { dg-do run }
!
! Test the fix for PR82312.f90
!
! Posted on Stack Overflow:
! https://stackoverflow.com/questions/46369744
! /gfortran-associates-wrong-type-bound-procedure/46388339#46388339
!
module minimalisticcase
    implicit none

    type, public :: DataStructure
        integer :: i
    contains
        procedure, pass :: init => init_data_structure
        procedure, pass :: a => beginning_of_alphabet
    end type

    type, public :: DataLogger
        type(DataStructure), pointer :: data_structure
        contains
                procedure, pass :: init => init_data_logger
                procedure, pass :: do_something => do_something
    end type

    integer :: ctr = 0

contains
    subroutine init_data_structure(self)
        implicit none
        class(DataStructure), intent(inout) :: self
        write(*,*) 'init_data_structure'
        ctr = ctr + 1
    end subroutine

    subroutine beginning_of_alphabet(self)
        implicit none
        class(DataStructure), intent(inout) :: self

        write(*,*) 'beginning_of_alphabet'
        ctr = ctr + 10
    end subroutine

    subroutine init_data_logger(self, data_structure)
        implicit none
        class(DataLogger), intent(inout) :: self
        class(DataStructure), target :: data_structure
        write(*,*) 'init_data_logger'
        ctr = ctr + 100

        self%data_structure => data_structure ! Invalid change of 'self' vptr
        call self%do_something()
    end subroutine

    subroutine do_something(self)
        implicit none
        class(DataLogger), intent(inout) :: self

        write(*,*) 'do_something'
        ctr = ctr + 1000

    end subroutine
end module

program main
    use minimalisticcase
    implicit none

    type(DataStructure) :: data_structure
    type(DataLogger) :: data_logger

    call data_structure%init()
    call data_structure%a()
    call data_logger%init(data_structure)

    if (ctr .ne. 1111) call abort
end program
