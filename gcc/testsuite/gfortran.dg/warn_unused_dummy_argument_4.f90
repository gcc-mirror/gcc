! { dg-do compile }
! { dg-options "-Wall" }
!
! PR fortran/57469
!
! Contributed by Vladimir Fuka
!
! Don't warn for unused dummy arguments when they are used in namelists
!
   subroutine read_command_line(line,a,b)
     character(*),intent(in) :: line
     intent(inout) :: a,b
     namelist /cmd/ a,b

     read(line,nml = cmd)
   end
