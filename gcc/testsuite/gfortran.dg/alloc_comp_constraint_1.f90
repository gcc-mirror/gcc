! { dg-do compile }
! { dg-options -std=f2003 }
! Check that we don't allow IO of NAMELISTs with types with allocatable
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
    namelist /blah/ a  ! This is allowed under F2003, but not F95
    ! The following require User Defined Derived Type I/O procedures.
    write (*, *) a  ! { dg-error "cannot have ALLOCATABLE components" }

    read (*, *) b  ! { dg-error "cannot have ALLOCATABLE components" }

end program main
