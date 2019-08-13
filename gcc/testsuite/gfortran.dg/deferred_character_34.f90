! { dg-do run }
! PR fortran/90561
! This used to ICE.
! Original test case by Gerhard Steinmetz.
program p
   character(:), allocatable :: z(:)
   z = [character(2):: 'ab', 'xy']
   z = z(2)
   if (any(z /= 'xy')) stop 1
end
