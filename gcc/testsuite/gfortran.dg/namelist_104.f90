! { dg-do run { target fd_truncate } }
! { dg-shouldfail "Missing quote" }
!! PR118793 - Expanded namelist error messages
!
! Based on a testcase by Harald Anlauf
program namelist_104
  implicit none
  integer      :: unit = 10
  character(8) :: c1, c2
  namelist /tovs_obs_chan/ c1, c2
  open (unit ,file="nml-quotes-bug-104.nml")
  write(unit,*) "&tovs_obs_chan"
  write(unit,*) "  c1 = '1', c1 = '1',"
  write(unit,*) "  c1 = '1', c1 = '1',  c2 = 2 ,"
  write(unit,*) "/"
  rewind(unit)
  read (unit ,nml=tovs_obs_chan)
  close(unit ,status="delete")
end program namelist_104
! { dg-output "Missing quote while reading item 5 at line 3, column 30 in file .*(\r*\n+)" }
! { dg-output "   c1 = '1', c1 = '1',  c2 = 2 ,(\r*\n+)                             \\^(\r*\n+)" }
