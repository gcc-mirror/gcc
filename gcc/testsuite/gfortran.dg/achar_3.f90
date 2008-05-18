! { dg-do compile }
! { dg-options "-Wall" }
program main
  print *,achar(-3)     ! { dg-error "negative" }
  print *,achar(200)    ! { dg-warning "outside of range" }
  print *,char(222+221) ! { dg-error "too large for the collating sequence" }
  print *,char(-44)     ! { dg-error "negative" }
  print *,iachar("ü")   ! { dg-warning "outside of range" }
end program main
