! PR fortran/120193
! { dg-do run }
! { dg-options "-g -funsigned" }
! { dg-skip-if "" { *-*-* }  { "*" } { "-O0" } }

program foo
  unsigned(kind=1) :: a(2), e
  unsigned(kind=2) :: b(2), f
  unsigned(kind=4) :: c(2), g
  unsigned(kind=8) :: d(2), h
  character(kind=1, len=1) :: i(2), j
  character(kind=4, len=1) :: k(2), l
  a = 97u_1	! { dg-final { gdb-test 24 "a" "d" } }
  b = 97u_2	! { dg-final { gdb-test 24 "b" "c" } }
  c = 97u_4	! { dg-final { gdb-test 24 "c" "b" } }
  d = 97u_8	! { dg-final { gdb-test 24 "d" "a" } }
  e = 97u_1	! { dg-final { gdb-test 24 "e" "97" } }
  f = 97u_2	! { dg-final { gdb-test 24 "f" "97" } }
  g = 97u_4	! { dg-final { gdb-test 24 "g" "97" } }
  h = 97u_8	! { dg-final { gdb-test 24 "h" "97" } }
  i = 'a'	! { dg-final { gdb-test 24 "i" "('a', 'a')" } }
  j = 'b'	! { dg-final { gdb-test 24 "j" "'b'" } }
  k = 'c'
  l = 'd'
  print *, a
end program
