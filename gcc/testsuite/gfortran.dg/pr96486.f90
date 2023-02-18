! { dg-do run }

program test
  implicit none
  character(0) :: value
  integer :: l, stat
  call get_environment_variable("PATH",value,length=l,status=stat)
  if (stat.ne.-1) stop 1
end program test
