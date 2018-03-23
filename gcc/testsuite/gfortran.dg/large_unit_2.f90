! { dg-do run }
! PR31201 Too large unit number generates wrong code 
! Test case by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
      integer :: i
      logical :: l
      character(len=60) :: s
      open(2_8*huge(0)+20_8,file="foo",iostat=i)
      if (i == 0) STOP 1
      open(2_8*huge(0)+20_8,file="foo",err=99)
      STOP 2
 99   inquire(unit=18,opened=l)
      if (l) STOP 3
      end
