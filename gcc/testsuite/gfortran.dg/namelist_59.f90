! { dg-do run }
! PR41192 NAMELIST input with just a comment ("&NAME ! comment \") error 
program cmdline
! comment by itself causes error in gfortran
   call process(' ')
   call process('i=10 , j=20 k=30 ! change all three values')
   call process(' ')
   call process('! change no values')! before patch this failed.
end program cmdline

subroutine process(string)
 implicit none
 character(len=*) :: string
 character(len=132) :: lines(3)
 character(len=255) :: message
 integer :: i=1,j=2,k=3
 integer ios
 namelist /cmd/ i,j,k
 lines(1)='&cmd'
 lines(2)=string
 lines(3)='/'

 read(lines,nml=cmd,iostat=ios,iomsg=message)
 if (ios.ne.0) STOP 1
end subroutine process
