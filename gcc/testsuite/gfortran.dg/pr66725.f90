! { dg-do compile }
! PR fortran/66725
!
program foo

   open(unit=1,access = 999)        ! { dg-error "ACCESS requires" }
   open(unit=1,action = 999)        ! { dg-error "ACTION requires" }
   open(unit=1,asynchronous = 999)  ! { dg-error "ASYNCHRONOUS requires" }
   open(unit=1,blank = 999)         ! { dg-error "BLANK requires" }
   open(unit=1,decimal = 999)       ! { dg-error "DECIMAL requires" }
   open(unit=1,delim = 999)         ! { dg-error "DELIM requires" }
   open(unit=1,encoding = 999)      ! { dg-error "ENCODING requires" }
   open(unit=1,form = 999)          ! { dg-error "FORM requires" }
   open(unit=1,pad = 999)           ! { dg-error "PAD requires" }
   open(unit=1,position = 999)      ! { dg-error "POSITION requires" }
   open(unit=1,round = 999)         ! { dg-error "ROUND requires" }
   open(unit=1,sign = 999)          ! { dg-error "SIGN requires" }
   open(unit=1,status = 999)        ! { dg-error "STATUS requires" }

   close(unit=1, status=999)        ! { dg-error "STATUS requires" }

   write (unit=1, asynchronous=257) ! { dg-error "ASYNCHRONOUS requires" }
   write (unit=1, delim=257)        ! { dg-error "DELIM requires" }
   write (unit=1, decimal=257)      ! { dg-error "DECIMAL requires" }
   write (unit=1, round=257)        ! { dg-error "ROUND requires" }
   write (unit=1, sign=257)         ! { dg-error "SIGN requires" }

   write (unit=1, blank=257)        ! { dg-error "BLANK requires" }
   write (unit=1, pad=257)          ! { dg-error "PAD requires" }

end program foo
