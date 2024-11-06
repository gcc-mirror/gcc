! This test belongs to submodule_1.f90
! It is references as additional source in that test.
! The two code fragments need to be in separate files to show
! the error of pr80235.

submodule (pr80235) pr80235_sub

contains
  module subroutine test()
    implicit none
    if (var%v /= 42) stop 1
  end subroutine
end submodule pr80235_sub

program pr80235_prg
  use pr80235
  
  implicit none

  var%v = 42
  call test()
end program
