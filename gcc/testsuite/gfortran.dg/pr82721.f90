! { dg-do compile }
! { dg-set-target-env-var MALLOC_PERTURB_ "165" }
! PR fortran/82721
! Reject a duplicate declaration without leaving the CHARACTER length
! expression from the failed statement on the namespace charlen list.

integer :: b
character(len(c)) :: b  ! { dg-error "already has basic type of INTEGER" }
end
