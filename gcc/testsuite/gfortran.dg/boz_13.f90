! { dg-do run }

! PR fortran/36214
! For BOZ-initialization of floats, the precision used to be wrong sometimes.

implicit none
   real, parameter :: r = 0.0
   real(kind=8), parameter :: rd = real (z'00000000&
                                          &402953FD', 8)

   if (real (z'00000000&
              &402953FD', 8) /= rd) call abort
end
