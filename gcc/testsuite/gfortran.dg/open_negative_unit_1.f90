! { dg-do run }
! PR48618 - Negative unit number in OPEN(...) is sometimes allowed
!
! Test originally from Janne Blomqvist in PR:
! http://gcc.gnu.org/bugzilla/show_bug.cgi?id=48618

program nutest
    implicit none
    logical l
    integer id, ios

    open(newunit=id, file="foo_open_negative_unit_1.txt", iostat=ios)
    if (ios /= 0) call abort

    open(id, file="bar.txt", iostat=ios)
    if (ios /= 0) call abort

    close(id, status="delete")

    open(unit=10, file="foo_open_negative_unit_1.txt", status="old", iostat=ios)
    if (ios /= 0) call abort

    close(10, status="delete")

    open(-10, file="foo_open_negative_unit_1.txt", iostat=ios)
    if (ios == 0) call abort

    inquire(file="foo_open_negative_unit_1.txt", exist=l)
    if (l) call abort
end program nutest
