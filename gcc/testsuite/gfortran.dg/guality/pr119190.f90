! PR debug/119190
! { dg-do run }
! { dg-options "-g" }

program foo
  integer :: ia, ib
  complex :: ci
  ia = 1
  ib = 2
  ci = cmplx(ia, ib)
  print *, ia
  print *, ib	! { dg-final { gdb-test 12 "ci" "(1,2)" } }
end program
