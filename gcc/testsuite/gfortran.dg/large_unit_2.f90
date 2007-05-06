! { dg-do run }
! PR31201 Too large unit number generates wrong code 
! Test case by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
      integer :: i
      logical :: l
      character(len=60) :: s
      open(2_8*huge(0)+20_8,file="foo",iostat=i)
      if (i == 0) call abort
      open(2_8*huge(0)+20_8,file="foo",err=99)
      call abort
 99   inquire(unit=18,opened=l)
      if (l) call abort
      end
