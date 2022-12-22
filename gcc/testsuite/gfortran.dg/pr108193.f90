! PR rtl-optimization/108193
! { dg-do compile { target pthread } }
! { dg-options "-O2 -fsplit-loops -ftree-parallelize-loops=2 -fno-tree-dominator-opts" }

subroutine foo (n, r)
  implicit none
  integer :: i, j, n
  real :: s, r(*)

  s = 0.0

  do j = 1, 2
     do i = j, n
        s = r(i)
     end do
  end do

  do i = 1, n
     do j = i, n
        s = s + 1
     end do
     r(i) = s
  end do
end subroutine foo
