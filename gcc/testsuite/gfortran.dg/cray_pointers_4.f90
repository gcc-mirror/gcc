! { dg-do compile }
! { dg-options "-fcray-pointer" }

subroutine err1
  integer :: in_common1, in_common2, v, w, equiv1, equiv2
  common /in_common1/ in_common1
  pointer (ipt1, in_common1)		! { dg-error "conflicts with COMMON" }
  pointer (ipt2, in_common2)
  common /in_common2/ in_common2	! { dg-error "conflicts with COMMON" }
  equivalence (v, equiv1)
  pointer (ipt3, equiv1)		! { dg-error "conflicts with EQUIVALENCE" }
  pointer (ipt4, equiv2)
  equivalence (w, equiv2)		! { dg-error "conflicts with EQUIVALENCE" }
end subroutine err1
