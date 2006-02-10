! { dg-do compile }
! Fallout from the patch for PR 14771
! Testcase by Erik Zeek
program test
    call bob(5)
contains
    subroutine bob(n)
        integer, intent(in) :: n
        character(len=n) :: temp1
        character(len=(n)) :: temp2 ! Fails here
    end subroutine bob
end program test
