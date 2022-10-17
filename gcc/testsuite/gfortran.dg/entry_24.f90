! { dg-do compile }
! PR fortran/87737 - improve check on function entry characteristics

function f() ! { dg-error "mismatched characteristics" }
  character(:), allocatable :: f
  character(1)              :: g
  f = 'f'
  return
entry g()
  g = 'g'
end

function f2() ! { dg-error "mismatched characteristics" }
  character(1)              :: f2
  character(1), allocatable :: g2
  f2 = 'f'
  return
entry g2()
  g2 = 'g'
end
