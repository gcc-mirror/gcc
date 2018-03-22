! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR 42647: Missed initialization/dealloc of allocatable scalar DT with allocatable component
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>

module m
type st
  integer , allocatable :: a1
end type st
type at
  integer , allocatable :: a2(:)
end type at

type t1
  type(st), allocatable :: b1
end type t1
type t2
  type(st), allocatable :: b2(:)
end type t2
type t3
  type(at), allocatable :: b3
end type t3
type t4
  type(at), allocatable :: b4(:)
end type t4
end module m

use m
block ! Start new scoping unit as otherwise the vars are implicitly SAVEd
type(t1) :: na1, a1, aa1(:)
type(t2) :: na2, a2, aa2(:)
type(t3) :: na3, a3, aa3(:)
type(t4) :: na4, a4, aa4(:)

allocatable :: a1, a2, a3, a4, aa1, aa2, aa3,aa4

if(allocated(a1)) STOP 1
if(allocated(a2)) STOP 2
if(allocated(a3)) STOP 3
if(allocated(a4)) STOP 4
if(allocated(aa1)) STOP 5
if(allocated(aa2)) STOP 6
if(allocated(aa3)) STOP 7
if(allocated(aa4)) STOP 8

if(allocated(na1%b1)) STOP 9
if(allocated(na2%b2)) STOP 10
if(allocated(na3%b3)) STOP 11
if(allocated(na4%b4)) STOP 12
end block
end

! { dg-final { scan-tree-dump-times "__builtin_free" 54 "original" } }
