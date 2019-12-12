! Check that we correctly simplify ISNAN
!
! { dg-do compile }
!
! { dg-options "-fno-range-check" }
! { dg-add-options ieee }

  implicit none
  real, parameter :: inf = 2 * huge(inf)
  real, parameter :: nan1 = 0. / 0.
  real, parameter :: nan2 = 1.5 * nan1
  real, parameter :: nan3 = inf / inf
  real, parameter :: nan4 = inf - inf
  real, parameter :: nan5 = 0. * inf
  real, parameter :: normal = 42.

  integer(kind=merge(4, 0, isnan(nan1))) :: a
  integer(kind=merge(4, 0, isnan(nan2))) :: b
  integer(kind=merge(4, 0, isnan(nan3))) :: c
  integer(kind=merge(4, 0, isnan(nan4))) :: d
  integer(kind=merge(4, 0, isnan(nan5))) :: e

  integer(kind=merge(0, 4, isnan(inf))) :: f
  integer(kind=merge(0, 4, isnan(-inf))) :: g
  integer(kind=merge(0, 4, isnan(normal))) :: h

  end
