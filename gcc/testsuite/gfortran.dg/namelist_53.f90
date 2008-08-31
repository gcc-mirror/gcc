! { dg-do run }
! PR36895 Namelist writing to internal files
  character(30) :: line
  namelist /stuff/ n
  n = 123
  line = ""
  write(line,nml=stuff)
  if (line.ne."&STUFF  N=        123,  /") call abort
  end 
