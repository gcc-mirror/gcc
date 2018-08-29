! { dg-do run }
! PR67367
program bug
   implicit none
   character(len=1) :: c
   character(len=256) :: message
   integer ios
   call system('[ -d junko.dir ] || mkdir junko.dir')
   open(unit=10, file='junko.dir',iostat=ios,action='read',access='stream')
   if (ios.ne.0) then
      call system('rmdir junko.dir')
      STOP 1
   end if
   read(10, iostat=ios) c
   if (ios.ne.21.and.ios.ne.0) then 
      close(10, status='delete')
      STOP 2
   end if
   close(10, status='delete')
end program bug
