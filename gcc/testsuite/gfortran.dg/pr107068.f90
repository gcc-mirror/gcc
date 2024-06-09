! { dg-do run }
program test
  implicit none
  integer :: error
  logical, dimension(3,3) :: flc,flp
  namelist/inputdata/flc, flp

  flc = .false.
  flp = .false.

  open(10, file="inputfile")
  write(10,*) "&INPUTDATA"
  write(10,*) " FLC = T, "
  write(10,*) " FLP(1,2) = T,"
  write(10,*) "/"
  rewind(10)
  !write(*, nml=inputdata)
  !open(10,file="inputfile")
  read(10,inputdata,iostat=error)
  close(10, status='delete')
  if (error /= 0) stop 20
end program test
