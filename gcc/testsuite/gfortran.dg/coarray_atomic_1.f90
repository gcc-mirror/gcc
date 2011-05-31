! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
!
! PR fortran/18918
!
! Diagnostic for atomic subroutines
!
use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none
integer(atomic_int_kind) :: a(1)[*]
logical(1) :: c[*]
integer(atomic_int_kind) :: b
logical(atomic_logical_kind) :: d, e[*]

call atomic_define(a, 7_2) ! { dg-error "must be a scalar" }
call atomic_ref(b, b) ! { dg-error "shall be a coarray" }

call atomic_define(c, 7) ! { dg-error "an integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }
call atomic_ref(d, a(1)) ! { dg-error "shall have the same type" }
call atomic_ref(.true., e) ! { dg-error "shall be definable" }
end
