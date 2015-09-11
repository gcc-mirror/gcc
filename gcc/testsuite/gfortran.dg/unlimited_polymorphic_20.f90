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
    if (trim (res) .NE. " 4") call abort ()
    call bar(ucp, res)
    if (trim (res) .NE. " 4") call abort ()

contains

    subroutine sub1(dcl, ilen)
        character(len=*), target :: dcl
        integer(4) :: ilen
        character(len=:), allocatable :: hlp
        class(*), pointer :: ucp

        ucp => dcl

        select type (ucp)
        type is (character(len=*))
            if (len(dcl) .NE. ilen) call abort ()
            if (len(ucp) .NE. ilen) call abort ()
            hlp = ucp
            if (len(hlp) .NE. ilen) call abort ()
        class default
            call abort()
        end select
    end subroutine

    subroutine sub2
        character(len=:), allocatable, target :: dcl
        class(*), pointer :: ucp

        dcl = "ttt"
        ucp => dcl

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. 3) call abort ()
        class default
            call abort()
        end select
    end subroutine

    subroutine sub3(ucp)
        character(len=:), allocatable :: hlp
        class(*), pointer :: ucp

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. 4) call abort ()
            hlp = ucp
            if (len(hlp) .ne. 4) call abort ()
        class default
            call abort()
        end select
    end subroutine

    subroutine sub4(ucp, ilen)
        character(len=:), allocatable :: hlp
        integer(4) :: ilen
        class(*) :: ucp

        select type (ucp)
        type is (character(len=*))
            if (len(ucp) .ne. ilen) call abort ()
            hlp = ucp
            if (len(hlp) .ne. ilen) call abort ()
        class default
            call abort()
        end select
    end subroutine
end program

