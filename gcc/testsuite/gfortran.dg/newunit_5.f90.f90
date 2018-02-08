! { dg-do run )
! PR83525 Combination of newunit and internal unit was failing.
program main
  integer :: funit
  logical :: isopen
  integer :: this, another
  character(len=:), allocatable :: message
  
  message = "12"
  read(message, *) this
  if (this.ne.12) call abort
  
  open(newunit=funit, status="scratch")
  write(funit, *) "13"
  rewind(funit)
  read(funit, *) another
  !write(*,*) another
  close(funit)
  if (another.ne.13) call abort
end
