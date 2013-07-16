! { dg-do run }
!
! PR fortran/56696
!
! Contributed by Keith Refson
!

program iotest
   character(len=258) :: inp = ' 1.0 1.0 1.0'
   character(len=7) :: inp2 = '1 2 3 4'
   integer :: ios
   real :: a1, a2, a3, a4

   read(inp2,*,iostat=ios) a1, a2, a3, a4
   if (ios /= 0) call abort ()

   read(inp,*,iostat=ios) a1, a2, a3, a4
   if (ios == 0) call abort ()
!   write(*,*) 'IOSTAT=',ios
end program iotest

