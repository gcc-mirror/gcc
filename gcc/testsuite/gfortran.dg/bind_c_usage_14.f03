! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/34079
! Bind(C) procedures shall have no character length
! dummy and actual arguments.
!

! SUBROUTINES

subroutine sub1noiso(a, b)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
end subroutine sub1noiso

subroutine sub2(a, b) bind(c)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
end subroutine sub2

! SUBROUTINES with ENTRY

subroutine sub3noiso(a, b)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
entry sub3noisoEntry(x,y,z)
  x = 'd'
end subroutine sub3noiso

subroutine sub4iso(a, b) bind(c)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
entry sub4isoEntry(x,y,z)
  x = 'd'
end subroutine sub4iso

subroutine sub5iso(a, b) bind(c)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
entry sub5noIsoEntry(x,y,z)
  x = 'd'
end subroutine sub5iso

subroutine sub6NoIso(a, b)
  use iso_c_binding
  implicit none
  character(len=1,kind=c_char) :: a(*), b
  character(len=1,kind=c_char):: x,z
  integer(c_int) :: y
  value :: b
  print *, a(1:2), b
entry sub6isoEntry(x,y,z)
  x = 'd'
end subroutine sub6NoIso

! The subroutines (including entry) should have
! only a char-length parameter if they are not bind(C).
!
! { dg-final { scan-tree-dump "sub1noiso \\(\[^.\]*a, \[^.\]*b, \[^.\]*_a, \[^.\]*_b\\)" "original" } }
! { dg-final { scan-tree-dump "sub2 \\(\[^.\]*a, \[^.\]*b\\)" "original" } }
! { dg-final { scan-tree-dump "sub3noiso \\(\[^.\]*a, \[^.\]*b, \[^.\]*_a, \[^.\]*_b\\)" "original" } }
! { dg-final { scan-tree-dump "sub3noisoentry \\(\[^.\]*x, \[^.\]*y, \[^.\]*z, \[^.\]*_x, \[^.\]*_z\\)" "original" } }
! { dg-final { scan-tree-dump "sub4iso \\(\[^.\]*a, \[^.\]*b\\)" "original" } }
! { dg-final { scan-tree-dump "sub4isoentry \\(\[^.\]*x, \[^.\]*y, \[^.\]*z, \[^.\]*_x, \[^.\]*_z\\)" "original" } }
! { dg-final { scan-tree-dump "sub5iso \\(\[^.\]*a, \[^.\]*b\\)" "original" } }
! { dg-final { scan-tree-dump "sub5noisoentry \\(\[^.\]*x, \[^.\]*y, \[^.\]*z, \[^.\]*_x, \[^.\]*_z\\)" "original" } }
! { dg-final { scan-tree-dump "sub6noiso \\(\[^.\]*a, \[^.\]*b, \[^.\]*_a, \[^.\]*_b\\)" "original" } }
! { dg-final { scan-tree-dump "sub6isoentry \\(\[^.\]*x, \[^.\]*y, \[^.\]*z, \[^.\]*_x, \[^.\]*_z\\)" "original" } }

! The master functions should have always a length parameter
! to ensure sharing a parameter between bind(C) and non-bind(C) works
!
! { dg-final { scan-tree-dump "master.0.sub3noiso \\(\[^.\]*__entry, \[^.\]*z, \[^.\]*y, \[^.\]*x, \[^.\]*b, \[^.\]*a, \[^.\]*_z, \[^.\]*_x, \[^.\]*_b, \[^.\]*_a\\)" "original" } }
! { dg-final { scan-tree-dump "master.1.sub4iso \\(\[^.\]*__entry, \[^.\]*z, \[^.\]*y, \[^.\]*x, \[^.\]*b, \[^.\]*a, \[^.\]*_z, \[^.\]*_x, \[^.\]*_b, \[^.\]*_a\\)" "original" } }
! { dg-final { scan-tree-dump "master.2.sub5iso \\(\[^.\]*__entry, \[^.\]*z, \[^.\]*y, \[^.\]*x, \[^.\]*b, \[^.\]*a, \[^.\]*_z, \[^.\]*_x, \[^.\]*_b, \[^.\]*_a\\)" "original" } }
! { dg-final { scan-tree-dump "master.3.sub6noiso \\(\[^.\]*__entry, \[^.\]*z, \[^.\]*y, \[^.\]*x, \[^.\]*b, \[^.\]*a, \[^.\]*_z, \[^.\]*_x, \[^.\]*_b, \[^.\]*_a\\)" "original" } }

! Thus, the master functions need to be called with length arguments
! present
!
! { dg-final { scan-tree-dump "master.0.sub3noiso .0, 0B, 0B, 0B, b, a, 0, 0, 1, 1\\);" "original" } }
! { dg-final { scan-tree-dump "master.0.sub3noiso .1, z, y, x, 0B, 0B, 1, 1, 0, 0\\);" "original" } }
! { dg-final { scan-tree-dump "master.1.sub4iso .0, 0B, 0B, 0B, b, a, 0, 0, 1, 1\\);" "original" } }
! { dg-final { scan-tree-dump "master.1.sub4iso .1, z, y, x, 0B, 0B, 1, 1, 0, 0\\);" "original" } }
! { dg-final { scan-tree-dump "master.2.sub5iso .0, 0B, 0B, 0B, b, a, 0, 0, 1, 1\\);" "original" } }
! { dg-final { scan-tree-dump "master.2.sub5iso .1, z, y, x, 0B, 0B, 1, 1, 0, 0\\);" "original" } }
! { dg-final { scan-tree-dump "master.3.sub6noiso .0, 0B, 0B, 0B, b, a, 0, 0, 1, 1\\);" "original" } }
! { dg-final { scan-tree-dump "master.3.sub6noiso .1, z, y, x, 0B, 0B, 1, 1, 0, 0\\);" "original" } }

! { dg-final { cleanup-tree-dump "original" } }
