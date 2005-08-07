! { dg-do run }
! PR 22390 Implement flush statement
program flush_1

   character(len=256) msg
   integer ios

   open (unit=10, access='SEQUENTIAL', status='SCRATCH')

   write (10, *) 42
   flush 10                   ! { dg-warning "Fortran 2003: FLUSH statement" }

   write (10, *) 42
   flush(10)                  ! { dg-warning "Fortran 2003: FLUSH statement" }

   write (10, *) 42
   flush(unit=10, iostat=ios) ! { dg-warning "Fortran 2003: FLUSH statement" }
   if (ios /= 0) call abort

   write (10, *) 42
   flush (unit=10, err=20)    ! { dg-warning "Fortran 2003: FLUSH statement" }
   goto 30
20 call abort
30 continue

   call flush(10)

end program flush_1
