! { dg-do compile }
! this is to simply test that the various ways the use statement can 
! appear are handled by the compiler, since i did a special treatment 
! of the intrinsic iso_c_binding module.  note: if the user doesn't 
! provide the 'intrinsic' keyword, the compiler will check for a user 
! provided module by the name of iso_c_binding before using the 
! intrinsic one.  --Rickett, 09.26.06
module use_stmt_0
  ! this is an error because c_ptr_2 does not exist 
  use, intrinsic :: iso_c_binding, only: c_ptr_2 ! { dg-error "Symbol 'c_ptr_2' referenced at \\(1\\) not found" }
end module use_stmt_0

module use_stmt_1
  ! this is an error because c_ptr_2 does not exist 
  use iso_c_binding, only: c_ptr_2 ! { dg-error "Symbol 'c_ptr_2' referenced at \\(1\\) not found" }
end module use_stmt_1

module use_stmt_2
  ! works fine
  use, intrinsic :: iso_c_binding, only: c_ptr
end module use_stmt_2

module use_stmt_3
  ! works fine
  use iso_c_binding, only: c_ptr
end module use_stmt_3

module use_stmt_4
  ! works fine
  use, intrinsic :: iso_c_binding
end module use_stmt_4

module use_stmt_5
  ! works fine
  use iso_c_binding
end module use_stmt_5

module use_stmt_6
  ! hmm, is this an error?  if so, it's not being caught...
  ! --Rickett, 09.13.06
  use, intrinsic :: iso_c_binding, only: c_int, c_int
end module use_stmt_6

module use_stmt_7
  ! hmm, is this an error?  if so, it's not being caught...
  ! --Rickett, 09.13.06
  use iso_c_binding, only: c_int, c_int
end module use_stmt_7

! { dg-final { cleanup-modules "use_stmt_2 use_stmt_3 use_stmt_4 use_stmt_5 use_stmt_6 use_stmt_7" } }
