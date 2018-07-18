! { dg-do run }
!
! Testing fix for PR fortran/60255
!
! Author: Andre Vehreschild <vehre@gmx.de>
!
MODULE m

contains
  subroutine bar (arg, res)
    class(*) :: arg
    character(100) :: res
    select type (w => arg)
      type is (character(*))
        write (res, '(I2)') len(w)
    end select
  end subroutine

END MODULE

program test
    use m;
    implicit none
    character(LEN=:), allocatable, target :: S
    character(LEN=100) :: res
    class(*), pointer :: ucp, ucp2
    call sub1 ("long test string", 16)
    call sub2 ()
    S = "test"
    ucp => S
    call sub3 (ucp)
    allocate (ucp2, source=ucp)
    call sub3 (ucp2)
    call sub4 (S, 4)
    call sub4 ("This is a longer string.", 24)
    call bar (S, res)
    if (trim (res) .NE. " 4") STOP 1
    call bar(ucp, res)
    if (trim (res) .NE. " 4") STOP 2

contains

    subroutine sub1(dcl, ilen)
        character(len=*), target :: dcl
        integer(4) :: ilen
        character(len=:), allocatable :: hlp
        class(*), pointer :: ucp

        ucp => dcl

        select type (ucp)
        type is (character(len=*))
            if (len(dcl) .NE. ilen) STOP 3
            if (len(ucp) .NE. ilen) STOP 4
            hlp = ucp
            if (len(hlp) .NE. ilen) STOP 5
        class default
            STOP 6
        end select
    end subroutine

    subroutine sub2
        character(len=:), allocatable, target :: dcl
        class(*), pointer :: ucp

        dcl = "ttt"
        ucp => dcl

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. 3) STOP 7
        class default
            STOP 8
        end select
    end subroutine

    subroutine sub3(ucp)
        character(len=:), allocatable :: hlp
        class(*), pointer :: ucp

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. 4) STOP 9
            hlp = ucp
            if (len(hlp) .ne. 4) STOP 10
        class default
            STOP 11
        end select
    end subroutine

    subroutine sub4(ucp, ilen)
        character(len=:), allocatable :: hlp
        integer(4) :: ilen
        class(*) :: ucp

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. ilen) STOP 12
            hlp = ucp
            if (len(hlp) .ne. ilen) STOP 13
        class default
            STOP 14
        end select
    end subroutine
end program

