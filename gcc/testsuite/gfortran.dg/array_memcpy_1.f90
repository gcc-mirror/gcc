! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine testi(a,b)
  integer :: a(20)
  integer :: b(20)
  a = b;
end subroutine

subroutine testr(a,b)
  real :: a(20)
  real :: b(20)
  a = b;
end subroutine

subroutine testz(a,b)
  complex :: a(20)
  complex :: b(20)
  a = b;
end subroutine

subroutine testl(a,b)
  logical :: a(20)
  logical :: b(20)
  a = b;
end subroutine

! { dg-final { scan-tree-dump-times "memcpy" 4 "original" } }
