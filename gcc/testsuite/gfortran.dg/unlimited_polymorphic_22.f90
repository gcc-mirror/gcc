! { dg-do run }
! Testing fix for PR fortran/60289
! Contributed by: Andre Vehreschild <vehre@gmx.de>
!
program test
    implicit none

    class(*), pointer :: P
    integer :: string_len = 10 *2

    allocate(character(string_len)::P)

    select type(P)
        type is (character(*))
            P ="some test string"
            if (P .ne. "some test string") then
                call abort ()
            end if
            if (len(P) .ne. 20) then
                call abort ()
            end if
            if (len(P) .eq. len("some test string")) then
                call abort ()
            end if
        class default
            call abort ()
    end select

    deallocate(P)

    ! Now for kind=4 chars.

    allocate(character(len=20,kind=4)::P)

    select type(P)
        type is (character(len=*,kind=4))
            P ="some test string"
            if (P .ne. 4_"some test string") then
                call abort ()
            end if
            if (len(P) .ne. 20) then
                call abort ()
            end if
            if (len(P) .eq. len("some test string")) then
                call abort ()
            end if
        type is (character(len=*,kind=1))
            call abort ()
        class default
            call abort ()
    end select

    deallocate(P)


end program test
