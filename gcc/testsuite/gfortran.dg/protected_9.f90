! { dg-do compile }
! PR fortran/66052
!
!
! Original code from Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de>
module a
  contains
  protected x   ! { dg-error "only allowed in specification part" }
end module a

program p
   contains
   protected x  ! { dg-error "only allowed in specification part" }
end
