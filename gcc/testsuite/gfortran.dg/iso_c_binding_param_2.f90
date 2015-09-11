! { dg-do compile }
! { dg-options "-O -fdump-tree-optimized" }
!
! Check that the GNU additions to ISO_C_Binding are accepted
!
use, intrinsic :: iso_c_binding, only: c_int128_t
use, intrinsic :: iso_c_binding, only: c_int_least128_t
use, intrinsic :: iso_c_binding, only: c_int_fast128_t
use, intrinsic :: iso_c_binding, only: c_float128
use, intrinsic :: iso_c_binding, only: c_float128_complex
implicit none
if (c_int128_t         >= 0 .and. c_int128_t         /= 16) call unreachable()
if (c_int_least128_t   >= 0 .and. c_int_least128_t   <  16) call unreachable()
if (c_int_fast128_t    >= 0 .and. c_int_fast128_t    <  16) call unreachable()
if (c_float128         >= 0 .and. c_float128         /= 16) call unreachable()
if (c_float128_complex >= 0 .and. c_float128_complex /= 16) call unreachable()
end

! { dg-final { scan-tree-dump-times "unreachable" 0 "optimized" } }
