! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Before a bogus error (argument not simply contiguous)
! was printed instead of the rank mismatch
!
! PR fortran/18918
!
integer :: A[*]
call bar(A) ! { dg-error "Rank mismatch in argument" }
contains
  subroutine bar(x)
    integer :: x(1)[*]
  end subroutine bar
end
