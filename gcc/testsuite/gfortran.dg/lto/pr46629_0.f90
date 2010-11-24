! PR middle-end/46629
! { dg-lto-do assemble }
! { dg-lto-options {{ -O2 -flto -ftree-vectorize }} }
! { dg-lto-options {{ -O2 -flto -ftree-vectorize -march=x86-64 }} { target i?86-*-* x86_64-*-* } }

subroutine foo
  character(len=6), save :: c
  real, save :: d(0:100)
  integer, save :: x, n, i
  n = x
  print *, c
  do i = 2, n
    d(i) = -d(i-1)
  end do
end
