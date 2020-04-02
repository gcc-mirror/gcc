! { dg-do compile }
!
! PR fortran/93522
!
! Contributed by Shubham Narlawar

program rank_new
    implicit none
    integer :: some_var_assumed
    integer, DIMENSION(3,2,1) :: array
    PRINT *, RANK(array)
   call CALL_ME(array)
   contains
!No error expected
  subroutine CALL_ME23(x)
                implicit none
                integer:: x(..), a=10,b=20
                integer, dimension(10) :: arr = (/1,2,3,4,5/)  ! { dg-error "Different shape for array assignment at .1. on dimension 1 .10 and 5." }
                select rank(arr(1:3))  ! { dg-error "Syntax error in argument list" }
                RANK(1)  ! { dg-error "Unexpected RANK statement" }
                        print *, "1"
                 rank(2)  ! { dg-error "Unexpected RANK statement" }
                        print *, "2"
                end select  ! { dg-error "Expecting END SUBROUTINE statement" }
        end subroutine
end program
