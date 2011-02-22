! { dg-do compile }
! { dg-options "" }
!
! PR fortran/47542
!
integer, target, save :: tgt = 77
integer, pointer ::ptr_stmt  ! { dg-error "Statement function .ptr_stmt. at .1. may not have pointer or allocatable attribute" }
integer, allocatable :: alloc_stmt ! { dg-error "Statement function .alloc_stmt. at .1. may not have pointer or allocatable attribute" }

ptr_stmt() = tgt
alloc_stmt() = 78
end
