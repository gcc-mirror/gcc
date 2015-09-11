! { dg-do compile }
! { dg-options "-O -fdump-tree-original -finline-matmul-limit=0 -Warray-temporaries" }
program main
  implicit none
  real, dimension(2,2) :: a, b, c, d
  integer :: i
  real :: x, z
  character(60) :: line
  real, external :: ext_func
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

  data a /2., 3., 5., 7./
  data b /11., 13., 17., 23./
  write (unit=line, fmt='(4F7.2)') matmul(a,b)  &
       & + matmul(a,b)    ! { dg-warning "Creating array temporary" }
  z = sin(x) + cos(x) + sin(x) + cos(x)
  print *,z
  x = ext_func(a) + 23 + ext_func(a)
  print *,d,x
  z = element(x) + element(x)
  print *,z
  i = mypure(x) - mypure(x)
  print *,i
  z = elem_impure(x) - elem_impure(x)
  print *,z
end program main
! { dg-final { scan-tree-dump-times "matmul_r4" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_sinf" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_cosf" 1 "original" } }
! { dg-final { scan-tree-dump-times "ext_func" 2 "original" } }
! { dg-final { scan-tree-dump-times "element" 1 "original" } }
! { dg-final { scan-tree-dump-times "mypure" 1 "original" } }
! { dg-final { scan-tree-dump-times "elem_impure" 2 "original" } }
