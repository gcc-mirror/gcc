! { dg-do run }
!
! Tests that PR66408 stays fixed.
!
! Contributed by <werner.blokbuster@gmail.com>
!
module mytest

    implicit none

    type vary
        character(:), allocatable :: string
    end type vary

    interface assignment(=)
        module procedure char_eq_vary
    end interface assignment(=)

contains

    subroutine char_eq_vary(my_char,my_vary)
        character(:), allocatable, intent(out) :: my_char
        type(vary), intent(in) :: my_vary
        my_char = my_vary%string
    end subroutine char_eq_vary

end module mytest


program thistest

    use mytest, only: vary, assignment(=)
    implicit none

    character(:), allocatable :: test_char
    character(14), parameter :: str = 'example string'
    type(vary) :: test_vary
    type(vary) :: my_stuff


    test_vary%string = str
    if (test_vary%string .ne. str) call abort

! This previously gave a blank string.
    my_stuff%string = test_vary
    if (my_stuff%string .ne. str) call abort

    test_char = test_vary
    if (test_char .ne. str) call abort

    my_stuff = test_vary
    if (my_stuff%string .ne. str) call abort

end program thistest
