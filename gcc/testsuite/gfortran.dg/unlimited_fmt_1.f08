! { dg-do run }
! PR41075 Implement unlimited format item '*'.
! Contributed by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program unlimited
   implicit none
   integer i
   character(len=60) :: string
   integer, parameter :: n = 10
   integer, dimension(n) :: iarray
   iarray = (/ (i,i=1,n) /)
   do i=1,10
     write( string, '( "iarray =", *(g0, :, ","))') &
     & "abcdefg",iarray, i,"jklmnop"
   end do
   if (string.ne."iarray =abcdefg,1,2,3,4,5,6,7,8,9,10,10,jklmnop") &
   & call abort
end program unlimited
