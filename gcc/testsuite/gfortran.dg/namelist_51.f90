! { dg-do run }
! PR36676 Namelist comment problems
! test case from PR, reduced by Jerry DeLisle  <jvdelisle@gcc.gnu.org>
program mem_nml
    implicit none
    integer, save :: nxc
    nxc = 0
    call readNamelist()
contains
subroutine readNamelist()
implicit none
namelist /INPUT/    nxc
open(unit = 101, status="scratch")
write(101,'(a)')"&INPUT"
write(101,'(a)')""
write(101,'(a)')"!"
write(101,'(a)')"!"
write(101,'(a)')"!"
write(101,'(a)')"nxc = 100"
write(101,'(a)')"&END"
rewind(101)
read(unit = 101, nml = INPUT)
if (nxc /= 100) call abort
close(unit = 101)
endsubroutine
end program mem_nml

