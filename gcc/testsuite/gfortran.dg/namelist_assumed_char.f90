! { dg-do compile }
! PR30481 Assumed size character is not allowed in namelist.
! Test case from PR, submitted by Jerry DeLisle <jvdelisle@gcc.gnu.org>
subroutine foo(c)
  character*(*) c
  namelist /abc/ c  ! { dg-error "Assumed character length" }
end subroutine
