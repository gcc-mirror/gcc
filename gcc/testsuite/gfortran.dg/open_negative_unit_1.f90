! { dg-do run }
! PR48618 - Negative unit number in OPEN(...) is sometimes allowed
!
! Test originally from Janne Blomqvist in PR:
! http://gcc.gnu.org/bugzilla/show_bug.cgi?id=48618

program nutest
    implicit none
    integer id, ios

    open(newunit=id, file="foo.txt", iostat=ios)
    if (ios /= 0) call abort

    open(id, file="bar.txt", iostat=ios)
    if (ios /= 0) call abort

    close(id, status="delete")

    open(-10, file="foo.txt", iostat=ios)
    if (ios == 0) call abort
end program nutest
