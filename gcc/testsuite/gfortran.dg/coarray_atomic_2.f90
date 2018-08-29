! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008" }
!
use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none

intrinsic :: atomic_define
intrinsic :: atomic_ref
intrinsic :: atomic_cas ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_add ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_and ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_or ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_xor ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_fetch_add ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_fetch_and ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_fetch_or ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
intrinsic :: atomic_fetch_xor ! { dg-error "not available in the current standard settings but new in Fortran 2018." }
integer(atomic_int_kind) :: caf[*], var
logical(atomic_logical_kind) :: caf_log[*], var2
integer :: stat
integer(1) :: stat2

call atomic_define(caf, 5, stat=stat) ! { dg-error "STAT= argument to atomic_define" }
call atomic_define(caf_log, .true., stat=stat2) ! { dg-error "must be of kind 4" }
call atomic_ref(var, caf[1], stat=stat2) ! { dg-error "must be of kind 4" }
call atomic_ref(var2, caf_log[1], stat=stat) ! { dg-error "STAT= argument to atomic_ref" }
end
