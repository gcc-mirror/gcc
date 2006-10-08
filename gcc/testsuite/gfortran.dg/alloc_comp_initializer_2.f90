! { dg-do compile }
! Check that default initializer for allocatable components isn't accepted (PR
! 20541)
program main

    type :: foo
        integer, allocatable :: a(:) = [ 1 ] ! { dg-error "Initialization of allocatable" }

        integer :: x ! Just to avoid "extra" error messages about empty type.
    end type foo

end program main
