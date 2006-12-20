! { dg-do compile }
! { dg-options "-O2 -fdump-tree-original" }
subroutine i1(a)
  integer :: a(20)
  a = 0;
end subroutine

subroutine i2(a)
  integer :: a(20)
  a(:) = 0;
end subroutine

subroutine i3(a)
  integer :: a(20)
  a(1:20) = 0;
end subroutine

subroutine r1(a)
  real :: a(20)
  a = 0.0;
end subroutine

subroutine r2(a)
  real :: a(20)
  a(:) = 0.0;
end subroutine

subroutine r3(a)
  real :: a(20)
  a(1:20) = 0.0;
end subroutine

subroutine z1(a)
  complex :: a(20)
  a = 0;
end subroutine

subroutine z2(a)
  complex :: a(20)
  a(:) = 0;
end subroutine

subroutine z3(a)
  complex :: a(20)
  a(1:20) = 0;
end subroutine

subroutine l1(a)
  logical :: a(20)
  a = .false.;
end subroutine

subroutine l2(a)
  logical :: a(20)
  a(:) = .false.;
end subroutine

subroutine l3(a)
  logical :: a(20)
  a(1:20) = .false.;
end subroutine

! { dg-final { scan-tree-dump-times "memset" 12 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
