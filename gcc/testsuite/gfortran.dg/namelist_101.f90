! { dg-do run { target fd_truncate } }
! { dg-shouldfail "Missing quote" }
!
! PR libfortran/118793
program nml_quotes_bug
  implicit none
  integer      :: unit = 10
  character(8) :: c1, c2
  namelist /tovs_obs_chan/ c1, c2
  open (unit ,file="nml-quotes-bug.nml")
  write(unit,*) "&tovs_obs_chan"
  write(unit,*) "  c1 = '1',"
  write(unit,*) "  c2 =  2a ,"
  write(unit,*) "/"
  rewind(unit)
  read (unit ,nml=tovs_obs_chan)
  close(unit ,status="delete")
end program nml_quotes_bug
! { dg-output "Missing quote while reading item 2 at line 3, column 10 in file .*(\r*\n+)" }
! { dg-output "   c2 =  2a ,(\r*\n+)         \\^(\r*\n+)" }
