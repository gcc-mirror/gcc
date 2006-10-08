! { dg-do compile }
! Check that equivalence with allocatable components isn't allowed (PR 20541)
program main

    type :: foo
        sequence
        integer, allocatable :: x(:)
    end type foo

    type(foo) :: a
    integer :: b

    equivalence (a, b) ! { dg-error "cannot have ALLOCATABLE components" }

end program main
