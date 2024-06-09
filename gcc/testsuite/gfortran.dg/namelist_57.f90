! { dg-do run }
! PR37294 Namelist I/O to array character internal units.
! Test case from adapted from PR by Jerry DeLisle <jvdelisle@gcc.gnu.org>
  character(30) :: line(3)
  namelist /stuff/ n
  n = 123
  line = ""
  write(line,nml=stuff)
  if (line(1) .ne. " &STUFF") STOP 1
  if (line(2) .ne. " N=123        ,") STOP 2
  if (line(3) .ne. " /") STOP 3
  end 
