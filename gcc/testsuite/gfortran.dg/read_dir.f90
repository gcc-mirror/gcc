! { dg-do run }
! PR67367
program bug
   implicit none
   character(len=1) :: c
   character(len=256) :: message
   integer ios
   call system('[ -d junko.dir ] || mkdir junko.dir')
   open(unit=10, file='junko.dir',iostat=ios,action='read',access='stream')
   if (ios.ne.0) call abort
   read(10, iostat=ios) c
   if (ios.ne.21) call abort
   call system('rmdir junko.dir')
end program bug
