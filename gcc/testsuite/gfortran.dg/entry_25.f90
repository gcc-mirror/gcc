! { dg-do compile }
! PR fortran/102311 - ICE during error recovery checking entry characteristics

module m
contains
  function f() ! { dg-error "mismatched characteristics" }
    character(:), allocatable :: f
    character(1)              :: g
    f = 'f'
  entry g()
    g = 'g'
  end
end
