! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/41580
!
! Compile-time simplification of SAME_TYPE_AS and EXTENDS_TYPE_OF.

implicit none
type t1
  integer :: a
end type t1
type, extends(t1):: t11
  integer :: b
end type t11
type, extends(t11):: t111
  integer :: c
end type t111
type t2
  integer :: a
end type t2

type(t1) a1
type(t11) a11
type(t2) a2
class(t1), allocatable :: b1
class(t11), allocatable :: b11
class(t2), allocatable :: b2

logical, parameter :: p1 = same_type_as(a1,a2)  ! F
logical, parameter :: p2 = same_type_as(a2,a1)  ! F
logical, parameter :: p3 = same_type_as(a1,a11) ! F
logical, parameter :: p4 = same_type_as(a11,a1) ! F
logical, parameter :: p5 = same_type_as(a11,a11)! T
logical, parameter :: p6 = same_type_as(a1,a1)  ! T

if (p1 .or. p2 .or. p3 .or. p4 .or. .not. p5 .or. .not. p6) call should_not_exist()

if (same_type_as(b1,b1)   .neqv. .true.) call should_not_exist()

! Not (trivially) compile-time simplifiable:
if (same_type_as(b1,a1)  .neqv. .true.) STOP 1
if (same_type_as(b1,a11) .neqv. .false.) STOP 2
allocate(t1 :: b1)
if (same_type_as(b1,a1)  .neqv. .true.) STOP 3
if (same_type_as(b1,a11) .neqv. .false.) STOP 4
deallocate(b1)
allocate(t11 :: b1)
if (same_type_as(b1,a1)  .neqv. .false.) STOP 5
if (same_type_as(b1,a11) .neqv. .true.) STOP 6
deallocate(b1)


! .true. -> same type
if (extends_type_of(a1,a1)   .neqv. .true.) call should_not_exist()
if (extends_type_of(a11,a11) .neqv. .true.) call should_not_exist()
if (extends_type_of(a2,a2)   .neqv. .true.) call should_not_exist()

! .false. -> type compatibility possible
if (extends_type_of(a1,a2)  .neqv. .false.) call should_not_exist()
if (extends_type_of(a2,a1)  .neqv. .false.) call should_not_exist()
if (extends_type_of(a11,a2) .neqv. .false.) call should_not_exist()
if (extends_type_of(a2,a11) .neqv. .false.) call should_not_exist()

if (extends_type_of(b1,b2)  .neqv. .false.) call should_not_exist()
if (extends_type_of(b2,b1)  .neqv. .false.) call should_not_exist()
if (extends_type_of(b11,b2) .neqv. .false.) call should_not_exist()
if (extends_type_of(b2,b11) .neqv. .false.) call should_not_exist()

if (extends_type_of(b1,a2)  .neqv. .false.) call should_not_exist()
if (extends_type_of(b2,a1)  .neqv. .false.) call should_not_exist()
if (extends_type_of(b11,a2) .neqv. .false.) call should_not_exist()
if (extends_type_of(b2,a11) .neqv. .false.) call should_not_exist()

if (extends_type_of(a1,b2)  .neqv. .false.) call should_not_exist()
if (extends_type_of(a2,b1)  .neqv. .false.) call should_not_exist()
if (extends_type_of(a11,b2) .neqv. .false.) call should_not_exist()
if (extends_type_of(a2,b11) .neqv. .false.) call should_not_exist()

! type extension possible, compile-time checkable
if (extends_type_of(a1,a11) .neqv. .false.) call should_not_exist()
if (extends_type_of(a11,a1) .neqv. .true.) call should_not_exist()

if (extends_type_of(b1,a1)   .neqv. .true.) call should_not_exist()
if (extends_type_of(b11,a1)  .neqv. .true.) call should_not_exist()
if (extends_type_of(b11,a11) .neqv. .true.) call should_not_exist()

if (extends_type_of(a1,b11)  .neqv. .false.) call should_not_exist()


! Special case, simplified at tree folding:
if (extends_type_of(b1,b1)   .neqv. .true.) STOP 7

! All other possibilities are not compile-time checkable
if (extends_type_of(b11,b1)  .neqv. .true.) STOP 8
if (extends_type_of(b1,b11)  .neqv. .false.) STOP 9
if (extends_type_of(a11,b11) .neqv. .true.) STOP 10

allocate(t11 :: b11)
if (extends_type_of(a11,b11) .neqv. .true.) STOP 11
deallocate(b11)

allocate(t111 :: b11)
if (extends_type_of(a11,b11) .neqv. .false.) STOP 12
deallocate(b11)

allocate(t11 :: b1)
if (extends_type_of(a11,b1) .neqv. .true.) STOP 13
deallocate(b1)

allocate(t11::b1)
if (extends_type_of(b1,a11) .neqv. .true.) STOP 14
deallocate(b1)

allocate(b1,source=a11)
if (extends_type_of(b1,a11) .neqv. .true.) STOP 15
deallocate(b1)

allocate( b1,source=a1)
if (extends_type_of(b1,a11) .neqv. .false.) STOP 16
deallocate(b1)

end

! { dg-final { scan-tree-dump-times "stop_numeric" 16 "original" } }
! { dg-final { scan-tree-dump-times "should_not_exist" 0 "original" } }
