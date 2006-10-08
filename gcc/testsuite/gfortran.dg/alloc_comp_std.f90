! { dg-do compile }
! { dg-options "-std=f95" }
!
! Check that we don't accept allocatable components for -std=f95 (PR 20541)
!
program main

    type :: foo
        integer, allocatable :: bar(:) ! { dg-error "ALLOCATABLE attribute" }

        integer :: x ! Just to avoid "extra" error messages about empty type.
    end type foo

end program main
