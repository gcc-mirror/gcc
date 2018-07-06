program test
  common /bar/ c, d
  integer(4) :: c, d
interface
  subroutine foo()
  end subroutine
end interface
  call foo
  if (c/=1 .or. d/=2) STOP 1
end program test

