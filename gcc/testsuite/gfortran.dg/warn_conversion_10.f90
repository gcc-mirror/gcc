! { dg-do compile }
! { dg-options "-fno-range-check -Wconversion" }
! PR 88298 - this used to warn unnecessarily.  Original test case by
! Harald Anlauf.
subroutine bug (j, js)
  integer    :: j, js(3,2)
  js(:,:) = cshift (js(:,:), shift=j, dim=1)
end subroutine bug
