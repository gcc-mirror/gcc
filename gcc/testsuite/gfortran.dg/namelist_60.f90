! { dg-do run }
! PR42901  Reading array of structures from namelist
! Test case derived from the reporters test case.
program test_nml
type field_descr
  integer number
end type
type fsetup
  type (field_descr), dimension(3) :: vel ! 3 velocity components
end type
type (fsetup) field_setup
namelist /nl_setup/ field_setup
field_setup%vel%number = 0
! write(*,nml=nl_setup)
open(10, status="scratch")
write(10,'(a)') "&nl_setup"
write(10,'(a)') " field_setup%vel(1)%number=  3,"
write(10,'(a)') " field_setup%vel(2)%number=  9,"
write(10,'(a)') " field_setup%vel(3)%number=  27,"
write(10,'(a)') "/"
rewind(10)
read(10,nml=nl_setup)
if (field_setup%vel(1)%number .ne. 3) call abort
if (field_setup%vel(2)%number .ne. 9) call abort
if (field_setup%vel(3)%number .ne. 27) call abort
! write(*,nml=nl_setup)
end program test_nml
