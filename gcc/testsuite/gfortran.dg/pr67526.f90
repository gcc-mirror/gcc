! { dg-do compile }
! Original code from gerhard dot steinmetz dot fortran at t-online dot de
! PR fortran/67526
program p
   character :: c1 = 'abc'(:     ! { dg-error "error in SUBSTRING" }
   character :: c2 = 'abc'(3:    ! { dg-error "error in SUBSTRING" }
   character :: c3 = 'abc'(:1    ! { dg-error "error in SUBSTRING" }
   character :: c4 = 'abc'(2:2   ! { dg-error "error in SUBSTRING" }
end
