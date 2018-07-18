! { dg-do run }
! PR29563 Internal read loses data.
! Test case submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
! Without patch, values get muddled.
program pr29563
  character(len=4), dimension(3)::arraydata = (/'1123',' 456','789 '/)
  real(kind=8), dimension(3) :: tmp
  read(arraydata,*,iostat=iostat)tmp
  if (tmp(1).ne.1123.0) STOP 1
  if (tmp(2).ne.456.0) STOP 2
  if (tmp(3).ne.789.0) STOP 3
end program pr29563