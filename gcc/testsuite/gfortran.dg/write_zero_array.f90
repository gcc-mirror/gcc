! { dg-do run }
! PR30145 write statement fails to ignore zero-sized array
! Test case from PR, submitted by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program zeros
 implicit none
 character(20) :: msg = ""
 integer :: itemp(10) = 0
 integer :: ics
 !This was OK
 write(msg,*) 'itemp(6:0) = ',itemp(6:0),'a'
 if (msg /= " itemp(6:0) = a") STOP 1
 !This did not work before patch, segfaulted
 ics=6
 write(msg,*) 'itemp(ics:0) = ',itemp(ics:0),'a'
 if (msg /= " itemp(ics:0) = a") STOP 2
end program zeros

