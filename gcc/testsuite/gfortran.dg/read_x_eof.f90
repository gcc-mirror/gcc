! { dg-do run }
! PR43265 No EOF condition if reading with '(x)' from an empty file
! Test case from the reporter.
program pr43265
implicit none
integer::i
open(23,status="scratch")
write(23,'(a)') "Line 1"
write(23,'(a)') "Line 2"
write(23,'(a)') "Line 3"
rewind(23)
do i=1,10
  read(23,'(1x)',end=12)
enddo
12 if (i.ne.4) STOP 1
end
