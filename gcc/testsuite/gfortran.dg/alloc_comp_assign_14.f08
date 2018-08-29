! { dg-do run }
! Test for allocatable scalar components and deferred length char arrays.
! Check that fix for pr61275 works.
! Contributed by Antony Lewis <antony@cosmologist.info> and
!                Andre Vehreschild <vehre@gmx.de>
!
module typeA
    Type A
        integer :: X
        integer, allocatable :: y
        character(len=:), allocatable :: c
    end type A
end module

program test_allocatable_components
    use typeA
    Type(A) :: Me
    Type(A) :: Ea

    Me= A(X= 1, Y= 2, C="correctly allocated")

    if (Me%X /= 1) STOP 1
    if (.not. allocated(Me%y) .or. Me%y /= 2) STOP 2
    if (.not. allocated(Me%c)) STOP 3
    if (len(Me%c) /= 19) STOP 4
    if (Me%c /= "correctly allocated") STOP 5

    ! Now check explicitly allocated components.
    Ea%X = 9
    allocate(Ea%y)
    Ea%y = 42
    ! Implicit allocate on assign in the next line
    Ea%c = "13 characters"

    if (Ea%X /= 9) STOP 6
    if (.not. allocated(Ea%y) .or. Ea%y /= 42) STOP 7
    if (.not. allocated(Ea%c)) STOP 8
    if (len(Ea%c) /= 13) STOP 9
    if (Ea%c /= "13 characters") STOP 10

    deallocate(Ea%y)
    deallocate(Ea%c)
    if (allocated(Ea%y)) STOP 11
    if (allocated(Ea%c)) STOP 12
end program

