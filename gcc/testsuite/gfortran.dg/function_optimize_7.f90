! { dg-do compile }
! { dg-options "-O -fdump-tree-original -Warray-temporaries" }
subroutine xx(n, m, a, b, c, d, x, z, i, s_in, s_out)
  implicit none
  integer, intent(in) :: n, m
  real, intent(in), dimension(n,n) :: a, b, c
  real, intent(out), dimension(n,n) :: d
  real, intent(in), dimension(n,m) :: s_in
  real, intent(out), dimension(m) :: s_out
  integer, intent(out) :: i
  real, intent(inout) :: x
  real, intent(out) :: z
  character(60) :: line
  real, external :: ext_func
  integer :: one = 1
  interface
     elemental function element(x)
       real, intent(in) :: x
       real :: elem
     end function element
     pure function mypure(x)
       real, intent(in) :: x
       integer :: mypure
     end function mypure
     elemental impure function elem_impure(x)
       real, intent(in) :: x
       real :: elem_impure
     end function elem_impure
  end interface

  d = matmul(a,b) + matmul(a,b)    ! { dg-warning "Creating array temporary" }
  z = sin(x) + cos(x) + sin(x) + cos(x)
  x = ext_func(a) + 23 + ext_func(a)
  z = element(x) + element(x)
  i = mypure(x) - mypure(x)
  z = elem_impure(x) - elem_impure(x)
  s_out = sum(s_in,one) + 3.14 / sum(s_in,one) ! { dg-warning "Creating array temporary" }
end subroutine xx
! { dg-final { scan-tree-dump-times "matmul_r4" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_sinf" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_cosf" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_func" 2 "original" } }
! { dg-final { scan-tree-dump-times "element" 1 "original" } }
! { dg-final { scan-tree-dump-times "mypure" 1 "original" } }
! { dg-final { scan-tree-dump-times "elem_impure" 2 "original" } }
! { dg-final { scan-tree-dump-times "sum_r4" 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
