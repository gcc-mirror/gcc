! { dg-do run }
!
! PR fortran/57365
! [OOP] Sourced allocation fails with unlimited polymorphism
! Contributed by <rxs@hotmail.de>
!
program bug

    implicit none
    character(len=:), allocatable :: test

    test = "A test case"
    call allocate_test(test)
    deallocate(test)

contains

    subroutine allocate_test(var)
        class(*) :: var
        class(*), pointer :: copyofvar
        allocate(copyofvar, source=var)
        select type (copyofvar)
            type is (character(len=*))
!                print*, len(copyofvar), copyofvar
                if (len(copyofvar) /= 11) call abort ()
                if (copyofvar /= "A test case") call abort ()
        end select
        deallocate(copyofvar)
    end subroutine

end program bug
