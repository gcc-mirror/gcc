! { dg-do run }
! { dg-options "-O" }
! Test the fix for PR31011 in which the length of the array sections
! with stride other than unity were incorrectly calculated.
!
! Contributed by <terry@chem.gu.se>
!
program PotentialMatrix
 implicit none
 real(kind=8),dimension(2),parameter::v2=(/1,2/)
 real(kind=8),dimension(4),parameter::v4=(/1,2,3,4/)
 if (any (v2*v4(1:3:2) .ne. (/1,6/))) STOP 1
 if (any (v2*v4(3:1:-2) .ne. (/3,2/))) STOP 2
end
