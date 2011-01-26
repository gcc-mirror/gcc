! { dg-do compile }
! { dg-options "-std=f95" }

! PR30481 Assumed size character is not allowed in namelist.
! Test case from PR, submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
!
! Modifications for PR fortran/47339 / PR fortran/43062:
! Add -std=f95, add bar()
!
subroutine foo(c)
  character*(*) c
  namelist /abc/ c  ! { dg-error "nonconstant character length in namelist" }
end subroutine

subroutine bar(d,n)
  integer :: n
  character(len=n) d
  namelist /abcd/ d  ! { dg-error "nonconstant character length in namelist" }
end subroutine bar

