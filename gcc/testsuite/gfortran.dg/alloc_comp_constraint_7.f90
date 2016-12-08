! { dg-do compile }
! { dg-options -std=f95 }
! Check that we don't allow types with allocatable
program main

    type :: foo
        integer :: k
        integer, allocatable :: x(:) ! { dg-error "Fortran 2003: ALLOCATABLE" }
    end type foo

    type :: bar
        type(foo) :: x
    end type bar

    type(foo) :: a
    type(bar) :: b
    namelist /blah/ a

end program main
