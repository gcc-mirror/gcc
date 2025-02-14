! { dg-do compile }
!
! PR fortran/56378
! PR fortran/52426
!
! Contributed by David Sagan & Joost VandeVondele
!

module t
 use, intrinsic :: iso_c_binding
 interface fvec2vec
   module procedure int_fvec2vec
 end interface
contains
 function int_fvec2vec (f_vec, n) result (c_vec)
 integer f_vec(:)
 integer(c_int), target :: c_vec(n)
 end function int_fvec2vec
 subroutine lat_to_c (Fp, C) bind(c)
 integer, allocatable :: ic(:)
 call lat_to_c2 (c_loc(fvec2vec(ic, n1_ic))) ! { dg-error "Argument X at .1. to C_LOC shall have either the POINTER or the TARGET attribute" }
 end subroutine lat_to_c
end module

use iso_c_binding
type(c_ptr) :: i
i = c_loc([1]) ! { dg-error "Argument X at .1. to C_LOC shall have either the POINTER or the TARGET attribute" }
end
