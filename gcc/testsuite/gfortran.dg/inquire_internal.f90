! { dg-do run }
!  Test case from PR61933.
   use iso_fortran_env
   logical :: unit_exists
   integer :: tunit, istat
   istat = 0
   tunit=-1 ! Represents an internal unit.
   unit_exists = .true.
   inquire(unit=tunit, exist=unit_exists, iostat=istat)
   !print *, "Error Code is : ", IOSTAT_INQUIRE_INTERNAL_UNIT
   !print *, "IOSTAT Code is: ", istat
   !print *, tunit, unit_exists
   if (istat.ne.iostat_inquire_internal_unit) STOP 1
   if (unit_exists) STOP 2
END
