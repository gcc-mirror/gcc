! { dg-do run }
!
! PR fortran/93835 - the following code resulted in an ICE
!
program p
  if (any(findloc(shape(1), 1) .ne. 0)) stop 1
end

