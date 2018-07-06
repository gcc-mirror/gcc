! { dg-do  run }
! PR 83436 - this used to cause an error.
! Original test case by Daan van Vugt.
module mod_random_seed
  implicit none
contains
  !> Read an int from /dev/urandom
  subroutine read_urandom_int(seed, ierr)
    implicit none
    integer, intent(out) :: seed
    integer, intent(out) :: ierr
    integer :: un
    character(len=80) :: restart_file
    write(restart_file,'(A,A)') 'jorek', '_restart.h5'

    open(newunit=un, file="/dev/urandom", access="stream", &
        form="unformatted", action="read", status="old", iostat=ierr)
    if (ierr == 0) then
       read(un) seed
       close(un)
    end if
  end subroutine read_urandom_int
end module mod_random_seed

program test_random_seed
  use mod_random_seed
  implicit none
  integer :: seed, ierr
  call read_urandom_int(seed, ierr)
end program test_random_seed
