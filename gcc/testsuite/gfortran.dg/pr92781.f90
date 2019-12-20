! PR fortran/92781
! { dg-do compile }

function foo ()
  character(:), allocatable :: foo
  call bar ()
  foo = 'abc'
contains
  subroutine bar
  end
end
