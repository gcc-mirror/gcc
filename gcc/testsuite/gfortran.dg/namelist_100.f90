! { dg-do run )
! { dg-shouldfail "Missing quote" }
program nml_quotes_bug
  implicit none
  integer      :: unit = 10
  character(8) :: c1, c2
  namelist /tovs_obs_chan/ c1, c2
  open (unit ,file="nml-quotes-bug.nml")
  write(unit,*) "&tovs_obs_chan"
  write(unit,*) "  c1 = '1',"
  write(unit,*) "  c2 =  2 ,"
  write(unit,*) "/"
  rewind(unit)
  read (unit ,nml=tovs_obs_chan)
  close(unit ,status="delete")
end program nml_quotes_bug
