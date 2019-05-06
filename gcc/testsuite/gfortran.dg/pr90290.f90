! { dg-do compile }
! { dg-options "-std=f2008" }
program errorstop
  integer :: ec
  read *, ec
  stop ec      ! { dg-error "STOP code at " }
end program
