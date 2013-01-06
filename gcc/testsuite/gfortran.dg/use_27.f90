! { dg-do run }
!
! PR fortran/45900
! The BTYPEINSTANCE%CALLBACK() typebound call was resolved incorrectly to
! A's CALLBACK procedure instead of B's because the CALLBACK name is ambiguous
! in the MAIN namespace.
!
! Original testcase by someone <ortp21@gmail.com>

module A
implicit none
    type :: aType
    contains
        procedure :: callback
    end type aType
    contains
        subroutine callback( callback_, i )
            implicit none
            class(aType) :: callback_
            integer :: i

            i = 3
        end subroutine callback

        subroutine solver( callback_, i )
            implicit none
            class(aType) :: callback_
            integer :: i

            call callback_%callback(i)
        end subroutine solver
end module A

module B
use A, only: aType
implicit none
    type, extends(aType) :: bType
        integer :: i
    contains
        procedure :: callback
    end type bType
    contains
        subroutine callback( callback_, i )
            implicit none
            class(bType) :: callback_
            integer :: i

            i = 7
        end subroutine callback
end module B

program main
  call test1()
  call test2()

contains

  subroutine test1
    use A
    use B
    implicit none
    type(aType) :: aTypeInstance
    type(bType) :: bTypeInstance
    integer :: iflag

    bTypeInstance%i = 4

    iflag = 0
    call bTypeInstance%callback(iflag)
    if (iflag /= 7) call abort
    iflag = 1
    call solver( bTypeInstance, iflag )
    if (iflag /= 7) call abort

    iflag = 2
    call aTypeInstance%callback(iflag)
    if (iflag /= 3) call abort
  end subroutine test1

  subroutine test2
    use B
    use A
    implicit none
    type(aType) :: aTypeInstance
    type(bType) :: bTypeInstance
    integer :: iflag

    bTypeInstance%i = 4

    iflag = 0
    call bTypeInstance%callback(iflag)
    if (iflag /= 7) call abort
    iflag = 1
    call solver( bTypeInstance, iflag )
    if (iflag /= 7) call abort

    iflag = 2
    call aTypeInstance%callback(iflag)
    if (iflag /= 3) call abort
  end subroutine test2
end program main


