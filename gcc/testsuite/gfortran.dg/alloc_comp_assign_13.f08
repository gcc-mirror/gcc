! { dg-do run }
! Test for allocatable scalar components and deferred length char arrays.
! Check that fix for pr60357 works.
! Contributed by Antony Lewis <antony@cosmologist.info> and
!                Andre Vehreschild <vehre@gmx.de>
!
program test_allocatable_components
    Type A
        integer :: X
        integer, allocatable :: y
        character(len=:), allocatable :: c
    end type A
    Type(A) :: Me
    Type(A) :: Ea

    Me= A(X= 1, Y= 2, C="correctly allocated")

    if (Me%X /= 1) call abort()
    if (.not. allocated(Me%y) .or. Me%y /= 2) call abort()
    if (.not. allocated(Me%c)) call abort()
    if (len(Me%c) /= 19) call abort()
    if (Me%c /= "correctly allocated") call abort()

    ! Now check explicitly allocated components.
    Ea%X = 9
    allocate(Ea%y)
    Ea%y = 42
    ! Implicit allocate on assign in the next line
    Ea%c = "13 characters"

    if (Ea%X /= 9) call abort()
    if (.not. allocated(Ea%y) .or. Ea%y /= 42) call abort()
    if (.not. allocated(Ea%c)) call abort()
    if (len(Ea%c) /= 13) call abort()
    if (Ea%c /= "13 characters") call abort()

    deallocate(Ea%y)
    deallocate(Ea%c)
    if (allocated(Ea%y)) call abort()
    if (allocated(Ea%c)) call abort()
end program

! vim:ts=4:sts=4:sw=4:
