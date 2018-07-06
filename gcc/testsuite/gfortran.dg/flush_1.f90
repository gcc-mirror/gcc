! { dg-do run }
! PR 22390 Implement flush statement
program flush_1

   character(len=256) msg
   integer ios

   open (unit=10, access='SEQUENTIAL', status='SCRATCH')

   write (10, *) 42
   flush 10

   write (10, *) 42
   flush(10)

   write (10, *) 42
   flush(unit=10, iostat=ios)
   if (ios /= 0) STOP 1

   write (10, *) 42
   flush (unit=10, err=20)
   goto 30
20 STOP 2
30 continue

   call flush(10)

end program flush_1
