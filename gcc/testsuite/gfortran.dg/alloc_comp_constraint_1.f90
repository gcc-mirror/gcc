! { dg-do compile }
! Check that we don't allow IO or NAMELISTs with types with allocatable
! components (PR 20541)
program main

    type :: foo
        integer, allocatable :: x(:)
    end type foo

    type :: bar
        type(foo) :: x
    end type bar

    type(foo) :: a
    type(bar) :: b
    namelist /blah/ a ! { dg-error "has ALLOCATABLE or POINTER components and thus requires a defined input/output" }

    write (*, *) a  ! { dg-error "cannot have ALLOCATABLE components" }

    read (*, *) b  ! { dg-error "cannot have ALLOCATABLE components" }

end program main
