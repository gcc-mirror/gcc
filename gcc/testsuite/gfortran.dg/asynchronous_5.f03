! { dg-do compile }
! { dg-options "-std=f2003 -fdump-tree-original" }
!
! Covers code introduced by the fix to PR fortran/87923.
! The idea is that the variables in a namelist or I/O list used for
! asynchronous I/O will be marked with the asynchronous attribute.
!
! At this time, "asynchronous" is treated as "volatile" (see trans-decl.c).
! Thus, every variable referenced in an "asynchronous=yes" I/O list
! should obtain the "volatile" specifier in its declaration.
!

implicit none

type t
  character(4) :: comp_async
end type

type(t) :: dvar_async
integer :: ivar_async
real :: rvar_async
logical :: lvar_async
integer :: ivar_noasync

namelist /names/ ivar_async, rvar_async, lvar_async

open(1, asynchronous="yes")
write(1, asynchronous="yes") dvar_async
write(1, asynchronous="yes") dvar_async%comp_async
read(1, asynchronous="yes", nml=names)

open(2, asynchronous="no")
read(2, asynchronous="no") ivar_noasync

end

! { dg-final { scan-tree-dump "volatile +struct +\[^ \]+ +dvar_async" "original" } }
! { dg-final { scan-tree-dump "volatile +\[^ \]+ +ivar_async" "original" } }
! { dg-final { scan-tree-dump "volatile +\[^ \]+ +rvar_async" "original" } }
! { dg-final { scan-tree-dump "volatile +\[^ \]+ +lvar_async" "original" } }
! { dg-final { scan-tree-dump-not "volatile +\[^ \]+ +ivar_noasync" "original" } }
