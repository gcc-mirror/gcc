! { dg-do compile }
! Test of the patch for PR29941, in which LEN threw an error with
! an assumed size argument.
! 
! Contributed by William Mitchell <william.mitchell@nist.gov>
!
subroutine whatever(str)
character(len=*), dimension(*) :: str
integer :: i
i = len(str)
end subroutine whatever
