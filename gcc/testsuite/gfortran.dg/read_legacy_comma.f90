! { dg-do run }
! { dg-options "-std=legacy" }
! PR78351
program read_csv
  implicit none
  integer, parameter :: dbl = selected_real_kind(p=14, r=99)

  call checkit("101,1.,2.,3.,7,7")
  call checkit ("102,1.,,3.,,7")
  call checkit (",1.,,3.,,                                         ")

contains

subroutine checkit (text)
  character(*) :: text
  integer :: I1, I2, I3
  real(dbl) :: R1, R2, R3
  10 format (I8,3ES16.8,2I8)
  
  I1=-99;       I2=-99;       I3=-99
  R1=-99._DBL;  R2=-99._DBL;  R3=-99._DBL
  read(text,10) I1, R1, R2, R3, I2, I3
  if (I1 == -99) stop 1
  if (I2 == -99) stop 2
  if (I3 == -99) stop 3
  if (R1 == -99._DBL) stop 4
  if (R2 == -99._DBL) stop 5
  if (R3 == -99._DBL) stop 6
end subroutine

end program
