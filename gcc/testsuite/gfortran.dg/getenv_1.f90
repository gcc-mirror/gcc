! { dg-do run }
! Test the getenv and get_environment_variable intrinsics.
! Ignore the return value because it's not supported/meaningful on all targets
program getenv_1
  implicit none
  character(len=101) ::  var
  character(len=*), parameter :: home = 'HOME'
  integer :: len, stat
  call getenv(name=home, value=var)
  call get_environment_variable(name=home, value=var, &
       length=len, status=stat)
end program getenv_1
