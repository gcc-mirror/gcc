! { dg-do run }
! PR 22607: External/module pure return-by-reference functions

pure function hoj()
    integer :: hoj(3)
    hoj = (/1, 2, 3/)
end function hoj

module huj_mod
contains
    pure function huj()
        integer :: huj(3)
        huj = (/1, 2, 3/)
    end function huj
end module huj_mod

program pure_byref_3
    use huj_mod
    implicit none

    interface
        pure function hoj()
            integer :: hoj(3)
        end function hoj
    end interface
    integer :: a(3)

    a = huj()
    if (.not. all(a == (/1, 2, 3/))) STOP 1

    a = hoj()
    if (.not. all(a == (/1, 2, 3/))) STOP 2
end program pure_byref_3
