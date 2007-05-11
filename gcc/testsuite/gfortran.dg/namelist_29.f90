! { dg-do run }
! Checks the fix for PR30878, in which the inclusion
! of an implicit function result variable in a namelist
! would cause an error.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  character(80) :: buffer
  if (f1 (buffer) .ne. 42) call abort ()
CONTAINS
   INTEGER FUNCTION F1 (buffer)
     NAMELIST /mynml/ F1
     integer :: check
     character(80) :: buffer
     F1 = 42
     write (buffer, nml = mynml)
     F1 = 0
     READ (buffer, nml = mynml)
   end function
END
