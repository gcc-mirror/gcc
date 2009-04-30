! { dg-do run }
! PR 20661: Handle non-advancing I/O with iostat
! Test case by Walt Brainerd, The Fortran Company

program fc002
   character(len=1) :: c
   integer :: k,k2
   character(len=*), parameter :: f="(a)"
   open(11,status="scratch", iostat=k)
   if (k /= 0) call abort
   write(11,f) "x"
   rewind (11)
   read(11, f, advance="no", iostat=k) c
   if (k /= 0) call abort
   read(11, f, advance="no", iostat=k) c
   if (k >= 0) call abort
   read(11, f, advance="no", iostat=k2) c
   if (k2 >= 0 .or. k == k2) call abort
end program fc002
