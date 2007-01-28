! { dg-do compile }
! { dg-options "-Wall" }
program main
  print *,achar(-3)     ! { dg-warning "outside of range" }
  print *,achar(200)    ! { dg-warning "outside of range" }
  print *,char(222+221) ! { dg-error "outside of range" }
  print *,char(-44)     ! { dg-error "outside of range" }
  print *,iachar("ü")   ! { dg-warning "outside of range" }
end program main
