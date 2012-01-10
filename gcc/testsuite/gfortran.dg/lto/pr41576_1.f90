program test
  common /bar/ c, d
  integer(4) :: c, d
  call foo
  if (c/=1 .or. d/=2) call abort
end program test

