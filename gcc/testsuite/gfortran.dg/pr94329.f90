! PR tree-optimization/94329
! { dg-do compile }
! { dg-options "-O1 -fno-tree-loop-optimize -fwrapv -fcompare-debug" }

subroutine pr94329 (s, t)
  real :: s, t(:,:)
  do i = 1,3
    do j = 1,3
      s = t(i,j)
    end do
  end do
end
