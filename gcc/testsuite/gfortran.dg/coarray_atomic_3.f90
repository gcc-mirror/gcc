! { dg-do compile }
! { dg-options "-fcoarray=single -std=f2008ts -fmax-errors=200" }
!
use iso_fortran_env, only: atomic_int_kind, atomic_logical_kind
implicit none

intrinsic :: atomic_define
intrinsic :: atomic_ref
intrinsic :: atomic_cas
intrinsic :: atomic_add
intrinsic :: atomic_and
intrinsic :: atomic_or
intrinsic :: atomic_xor
intrinsic :: atomic_fetch_add
intrinsic :: atomic_fetch_and
intrinsic :: atomic_fetch_or
intrinsic :: atomic_fetch_xor
integer(atomic_int_kind) :: caf[*], var
logical(atomic_logical_kind) :: caf_log[*], var2
integer :: stat
integer(1) :: var3, caf0[*]
logical(1) :: var4, caf0_log[*]

call atomic_define(caf[1], 2_2, stat=stat)
call atomic_define(atom=caf_log[1], value=.false._2)
call atomic_define(caf_log[1], 2) ! { dg-error "shall have the same type as 'atom'" }
call atomic_define(var, 2_2, stat=stat) ! { dg-error "shall be a coarray or coindexed" }
call atomic_define(caf0, 2_2, stat=stat) ! { dg-error "integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }
call atomic_define(var2, 2_2, stat=stat) ! { dg-error "shall be a coarray or coindexed" }
call atomic_define(caf0_log, 2_2, stat=stat) ! { dg-error "integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }

call atomic_ref(var3, caf[1], stat=stat)
call atomic_ref(value=var4, atom=caf_log[1])
call atomic_ref(var, caf_log[1]) ! { dg-error "shall have the same type as 'atom'" }
call atomic_ref(var, var) ! { dg-error "shall be a coarray or coindexed" }
call atomic_ref(var, caf0) ! { dg-error "integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }
call atomic_ref(var, caf0_log) ! { dg-error "integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }

call atomic_cas(caf[1], var, 2_4, 1_1, stat=stat)
call atomic_cas(caf[1], var, 2_2, 1_1, stat=stat) ! { dg-error "'compare' argument of 'atomic_cas' intrinsic at .1. must be the same type and kind as 'atom'" }
call atomic_cas(caf[1], var3, 2_2, 1_1, stat=stat) ! { dg-error "'old' argument of 'atomic_cas' intrinsic at .1. must be the same type and kind as 'atom'" }
call atomic_cas(caf[1], var3, 2_4, .false._4, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_cas(caf0[1], var, 2_4, 1_1, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }
call atomic_cas(var, var, 2_4, 1_1, stat=stat) ! { dg-error "shall be a coarray or coindexed" }
call atomic_cas(caf_log[1], var2, .true._4, .false._1, stat=stat)
call atomic_cas(caf_log[1], var2, .true._2, .false._1, stat=stat) ! { dg-error "'compare' argument of 'atomic_cas' intrinsic at .1. must be the same type and kind as 'atom'" }
call atomic_cas(caf_log[1], var4, .true._4, .false._1, stat=stat) ! { dg-error "'old' argument of 'atomic_cas' intrinsic at .1. must be the same type and kind as 'atom'" }
call atomic_cas(caf_log[1], var4, .true._4, 4_4, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_cas(atom=caf0_log[1], old=var4, compare=.true._4, new=.false._4, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND or a logical of ATOMIC_LOGICAL_KIND" }
call atomic_cas(var2, var4, .true._4, .false._4, stat=stat) ! { dg-error "shall be a coarray or coindexed" }
call atomic_cas(caf[1], var, 2_4, 1_1, stat=var3) ! { dg-error "'stat' argument of 'atomic_cas' intrinsic at .1. must be of kind 4" }

call atomic_add(atom=caf, value=2_4, stat=stat)
call atomic_add(caf, 2_2, stat=stat)
call atomic_add(caf, .false._2, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_add(caf_log, .false._2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_add(var, 34._4) ! { dg-error "shall be a coarray or coindexed" }
call atomic_add(atom=caf, value=2_4, stat=var3) ! { dg-error "'stat' argument of 'atomic_add' intrinsic at .1. must be of kind 4" }

call atomic_and(caf, 2_4, stat=stat)
call atomic_and(atom=caf, value=2_2, stat=stat)
call atomic_and(caf, .false._2, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_and(caf_log, .false._2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_and(var, 34._4) ! { dg-error "shall be a coarray or coindexed" }
call atomic_and(caf, 2_4, stat=var3) ! { dg-error "'stat' argument of 'atomic_and' intrinsic at .1. must be of kind 4" }

call atomic_or(caf, value=2_4, stat=stat)
call atomic_or(atom=caf, value=2_2, stat=stat)
call atomic_or(caf, .false._2, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_or(caf_log, .false._2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_or(var, 34._4) ! { dg-error "shall be a coarray or coindexed" }
call atomic_or(caf, value=2_4, stat=var3) ! { dg-error "'stat' argument of 'atomic_or' intrinsic at .1. must be of kind 4" }

call atomic_xor(caf, 2_4, stat=stat)
call atomic_xor(atom=caf, value=2_2, stat=stat)
call atomic_xor(caf, .false._2, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_xor(caf_log, .false._2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_xor(var, 34._4) ! { dg-error "shall be a coarray or coindexed" }
call atomic_xor(caf, 2_4, stat=var3) ! { dg-error "'stat' argument of 'atomic_xor' intrinsic at .1. must be of kind 4" }

call atomic_fetch_add(atom=caf, value=2_4, old=var, stat=stat)
call atomic_fetch_add(caf, 2_2, var)
call atomic_fetch_add(caf, .false._2, var, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_fetch_add(caf_log, .false._2, var2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_fetch_add(var, 34._4, var) ! { dg-error "shall be a coarray or coindexed" }
call atomic_fetch_add(caf, 2_2, var3) ! { dg-error "must be the same type and kind as 'atom'" }
call atomic_fetch_add(atom=caf, value=2_4, old=var, stat=var3) ! { dg-error "'stat' argument of 'atomic_fetch_add' intrinsic at .1. must be of kind 4" }

call atomic_fetch_and(atom=caf, value=2_4, old=var, stat=stat)
call atomic_fetch_and(caf, 2_2, var)
call atomic_fetch_and(caf, .false._2, var, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_fetch_and(caf_log, .false._2, var2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_fetch_and(var, 34._4, var) ! { dg-error "shall be a coarray or coindexed" }
call atomic_fetch_and(caf, 2_2, var3) ! { dg-error "must be the same type and kind as 'atom'" }
call atomic_fetch_and(atom=caf, value=2_4, old=var, stat=var3) ! { dg-error "'stat' argument of 'atomic_fetch_and' intrinsic at .1. must be of kind 4" }

call atomic_fetch_or(atom=caf, value=2_4, old=var, stat=stat)
call atomic_fetch_or(caf, 2_2, var)
call atomic_fetch_or(caf, .false._2, var, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_fetch_or(caf_log, .false._2, var2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_fetch_or(var, 34._4, var) ! { dg-error "shall be a coarray or coindexed" }
call atomic_fetch_or(caf, 2_2, var3) ! { dg-error "must be the same type and kind as 'atom'" }
call atomic_fetch_or(atom=caf, value=2_4, old=var, stat=var3) ! { dg-error "'stat' argument of 'atomic_fetch_or' intrinsic at .1. must be of kind 4" }

call atomic_fetch_xor(atom=caf, value=2_4, old=var, stat=stat)
call atomic_fetch_xor(caf, 2_2, var)
call atomic_fetch_xor(caf, .false._2, var, stat=stat) ! { dg-error "shall have the same type as 'atom'" }
call atomic_fetch_xor(caf_log, .false._2, var2, stat=stat) ! { dg-error "shall be an integer of ATOMIC_INT_KIND" }
call atomic_fetch_xor(var, 34._4, var) ! { dg-error "shall be a coarray or coindexed" }
call atomic_fetch_xor(caf, 2_2, var3) ! { dg-error "must be the same type and kind as 'atom'" }
call atomic_fetch_xor(atom=caf, value=2_4, old=var, stat=var3) ! { dg-error "'stat' argument of 'atomic_fetch_xor' intrinsic at .1. must be of kind 4" }
end
