! { dg-do compile }
!
! PR fortran/83057 - OPEN without a filename and without STATUS='SCRATCH'
!                    could produce a warning

  open(newunit=iun,file="file")         ! this is ok
  open(newunit=jun,status="scratch")    ! this too
  open(newunit=lun)             ! { dg-error "NEWUNIT specifier must have" }
end
