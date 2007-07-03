! { dg-do compile }
! { dg-options "-std=f95" }

! PR fortran/25062
!
! F95: 14.1.2.1:
! "A common block name in a scoping unit also may be the name of any local
!  entity other than a named constant, intrinsic procedure, or a local variable
!  that is also an external function in a function subprogram."
!
! F2003: 16.2.1
! "A name that identifies a common block in a scoping unit shall not be used 
!  to identify a constant or an intrinsic procedure in that scoping unit. If
!  a local identifier is also the name of a common block, the appearance of
!  that name in any context other than as a common block name in a COMMON
!  or SAVE statement is an appearance of the local identifier."
!
function func1() result(res)
  implicit none
  real res, r
  common /res/ r ! { dg-error "is also a function result" }
end function func1
end
