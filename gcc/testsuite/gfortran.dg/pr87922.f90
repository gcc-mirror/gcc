! { dg-do compile }
! PR fortran/87922
subroutine p
   read(1, asynchronous=['no'])           ! { dg-error "must be scalar" }
   read(1, asynchronous=[character::])    ! { dg-error "must be scalar" }
end
subroutine q
   write(1, asynchronous=['no'])          ! { dg-error "must be scalar" }
   write(1, asynchronous=[character::])   ! { dg-error "must be scalar" }
end
