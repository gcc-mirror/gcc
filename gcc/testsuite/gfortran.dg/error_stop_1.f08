! { dg-do run }
program stopper
  real, dimension(5,5,5) :: i
  error stop size(i) ! { dg-shouldfail "ERROR STOP 125" }
end program stopper
