! { dg-do run }
!
! PR95195 - improve runtime error when writing a namelist to an unformatted file

program test
  character(len=11) :: my_form = 'unformatted'
  integer           :: i = 1, j = 2, k = 3
  character(80)     :: iomsg
  namelist /nml1/ i, j, k
  open  (unit=10, file='namelist_97.dat', form=my_form)
  write (unit=10, nml=nml1, iostat=iostat, iomsg=iomsg)
  close (unit=10, status='delete')
  if (iostat == 0) stop 1
  if (iomsg  /= "Namelist formatting for unit connected with FORM='UNFORMATTED'") &
       stop 2
end program test
