! { dg-do run }
! PR29563 Internal read loses data.
! Test from test case. Submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Without patch, last value in array was being skipped in the read.
program pr29563
  character(len=10), dimension(3)::arraydata = (/' 1 2 3',' 4 5 6',' 7 8 9'/)
  real(kind=8), dimension(3,3) :: tmp
  tmp = 0.0
  read(arraydata,*,iostat=iostat)((tmp(i,j),j=1,3),i=1,3)
  if (tmp(3,3)-9.0.gt.0.0000001) call abort()
end program pr29563