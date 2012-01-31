! { dg-do compile }
!
! PR fortran/51816
!
module m
end module m

use m, only: operator(/) ! { dg-error "Intrinsic operator '/' referenced at .1. not found in module 'm'" }
end

! { dg-final { cleanup-modules "m" } }
