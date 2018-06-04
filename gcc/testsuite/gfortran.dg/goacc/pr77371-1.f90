! PR fortran/77371
! { dg-do compile }
program p
  character(:), allocatable :: z
  !$acc parallel
  z = 'abc' 
  !$acc end parallel
  print *, z
end
