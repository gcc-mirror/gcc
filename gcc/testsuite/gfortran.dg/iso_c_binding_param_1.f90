! { dg-do compile }
! { dg-options "-std=f2008" }
!
! Check that the GNU additions to ISO_C_Binding are properly diagnosed
!
use, intrinsic :: iso_c_binding, only: c_int128_t ! { dg-error "is not in the selected standard" }
use, intrinsic :: iso_c_binding, only: c_int_least128_t ! { dg-error "is not in the selected standard" }
use, intrinsic :: iso_c_binding, only: c_int_fast128_t ! { dg-error "is not in the selected standard" }
use, intrinsic :: iso_c_binding, only: c_float128 ! { dg-error "is not in the selected standard" }
use, intrinsic :: iso_c_binding, only: c_float128_complex ! { dg-error "is not in the selected standard" }
implicit none
end
