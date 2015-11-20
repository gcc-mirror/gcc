! { dg-do compile }
! PR fortran/67803
! Original code submitted by Gerhard Steinmetz
! <gerhard dot steinmetz dot fortran at t-online dot de >
!
program p
  character(2) :: x(1)
  x = '0' // [character :: 1]       ! { dg-error "Incompatible typespec for" }
  x = '0' // [character :: 1.]      ! { dg-error "Incompatible typespec for" }
  x = '0' // [character :: 1d1]     ! { dg-error "Incompatible typespec for" }
  x = '0' // [character :: (0.,1.)] ! { dg-error "Incompatible typespec for" }
  x = '0' // [character :: .true.]  ! { dg-error "Incompatible typespec for" }
  x = '0' // [character :: null()]  ! { dg-error "Incompatible typespec for" }
end
