! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Covers code introduced by the fix to PR fortran/87923.
! The idea is that the variables in a namelist or I/O list used for
! asynchronous I/O will be marked with the asynchronous attribute.
!
! At this time, "asynchronous" is treated as "volatile" (see trans-decl.c).
! Thus, every variable referenced in an "asynchronous=yes" I/O list
! should obtain the "volatile" specifier in its declaration.
!

type t
  character(4) :: comp_async
end type

character(2) :: ccvar_async
type(t) :: dvar_async
integer :: ivar_async
real :: rvar_async
logical :: lvar_async
type(t), dimension(2) :: darrvar_async
integer :: ivar_noasync

namelist /names/ ivar_async, rvar_async, lvar_async

open(1, asynchronous="yes")
write(1, asynchronous="yes") dvar_async, ccvar_async
write(1, asynchronous="yes") dvar_async%comp_async, darrvar_async
read(1, asynchronous="yes", nml=names)

open(2, asynchronous="no")
read(2, asynchronous="no") ivar_noasync

end

! { dg-final { scan-tree-dump-times "volatile.*?ccvar_async" 1 "original" } }
! { dg-final { scan-tree-dump-times "volatile.*?dvar_async" 1 "original" } }
! { dg-final { scan-tree-dump-times "volatile.*?ivar_async" 1 "original" } }
! { dg-final { scan-tree-dump-times "volatile.*?rvar_async" 1 "original" } }
! { dg-final { scan-tree-dump-times "volatile.*?lvar_async" 1 "original" } }
! { dg-final { scan-tree-dump-times "volatile.*?darrvar_async" 1 "original" } }
! { dg-final { scan-tree-dump-not "volatile.*?ivar_noasync" "original" } }
