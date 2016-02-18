! { dg-do run }
! PR69668 Error reading namelist opened with DELIM='NONE' 
program namelist
  implicit none

  integer,parameter :: tabsz=10
  integer :: i
  character(len=10),dimension(tabsz) :: tab
  namelist/tab_nml/tab

  tab(:)='invalid'

  ! Create a temporary test namelist file
  open(unit=23,status='scratch',delim='none')
  write(23,*) "&tab_nml"
  write(23,*) "tab(1)='in1',"
  write(23,*) "tab(2)='in2'"
  write(23,*) "/"
  rewind(23)

  read(unit=23,nml=tab_nml)

  close(unit=23)

  if (tab(1).ne.'in1') call abort
  if (tab(2).ne.'in2') call abort
  if (any(tab(3:tabsz).ne.'invalid')) call abort

end program namelist
