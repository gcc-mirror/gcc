! { dg-do compile }
! PR fortran/66725
!
program foo

   open(unit=1,access = 999)        ! { dg-error "must be of type CHARACTER" }
   open(unit=1,action = 999)        ! { dg-error "must be of type CHARACTER" }
   open(unit=1,asynchronous = 999)  ! { dg-error "must be of type CHARACTER" }
   open(unit=1,blank = 999)         ! { dg-error "must be of type CHARACTER" }
   open(unit=1,decimal = 999)       ! { dg-error "must be of type CHARACTER" }
   open(unit=1,delim = 999)         ! { dg-error "must be of type CHARACTER" }
   open(unit=1,encoding = 999)      ! { dg-error "must be of type CHARACTER" }
   open(unit=1,form = 999)          ! { dg-error "must be of type CHARACTER" }
   open(unit=1,pad = 999)           ! { dg-error "must be of type CHARACTER" }
   open(unit=1,position = 999)      ! { dg-error "must be of type CHARACTER" }
   open(unit=1,round = 999)         ! { dg-error "must be of type CHARACTER" }
   open(unit=1,sign = 999)          ! { dg-error "must be of type CHARACTER" }
   open(unit=1,status = 999)        ! { dg-error "must be of type CHARACTER" }

   close(unit=1, status=999)        ! { dg-error "must be of type CHARACTER" }

   write (unit=1, asynchronous=257) ! { dg-error "must be of type CHARACTER" }
   write (unit=1, delim=257)        ! { dg-error "must be of type CHARACTER" }
   write (unit=1, decimal=257)      ! { dg-error "must be of type CHARACTER" }
   write (unit=1, round=257)        ! { dg-error "must be of type CHARACTER" }
   write (unit=1, sign=257)         ! { dg-error "must be of type CHARACTER" }

   write (unit=1, blank=257)        ! { dg-error "must be of type CHARACTER" }
   write (unit=1, pad=257)          ! { dg-error "must be of type CHARACTER" }

end program foo
