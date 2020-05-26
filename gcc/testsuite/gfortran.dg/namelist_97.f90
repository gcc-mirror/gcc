! { dg-do run }
! { dg-output "At line 12 .*" }
! { dg-shouldfail "Fortran runtime error: Namelist formatting .* FORM='UNFORMATTED'" }
!
! PR95195 - improve runtime error when writing a namelist to an unformatted file

program test
  character(len=11) :: my_form = 'unformatted'
  integer           :: i = 1, j = 2, k = 3
  namelist /nml1/ i, j, k
  open  (unit=10, file='test.dat', form=my_form)
  write (unit=10, nml=nml1)
  close (unit=10, status='delete')
end program test
