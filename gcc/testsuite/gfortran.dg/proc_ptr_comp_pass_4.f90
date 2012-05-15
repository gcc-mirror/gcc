! { dg-do compile }
!
! PR 39630: [F03] Procedure Pointer Components with PASS
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

module m

 type :: t0
  procedure() :: p0  ! { dg-error "POINTER attribute is required for procedure pointer component" }
 end type

 type :: t1
  integer :: i
  procedure(foo1), pointer :: f1  ! { dg-error "must be scalar" }
 end type

 type :: t2
  integer :: i
  procedure(foo2), pointer :: f2  ! { dg-error "may not have the POINTER attribute" }
 end type

 type :: t3
  integer :: i
  procedure(foo3), pointer :: f3  ! { dg-error "may not be ALLOCATABLE" }
 end type

 type :: t4
   procedure(),     pass(x), pointer :: f4  ! { dg-error "NOPASS or explicit interface required" }
   procedure(real), pass(y), pointer :: f5  ! { dg-error "NOPASS or explicit interface required" }
   procedure(foo6), pass(c), pointer :: f6  ! { dg-error "has no argument" }
 end type

 type :: t7
   procedure(foo7), pass, pointer :: f7  ! { dg-error "must have at least one argument" }
 end type

 type :: t8
   procedure(foo8), pass, pointer :: f8  ! { dg-error "must be of the derived type" }
 end type

contains

 subroutine foo1 (x1,y1)
  type(t1) :: x1(:)
  type(t1) :: y1
 end subroutine

 subroutine foo2 (x2,y2)
  type(t2),pointer :: x2
  type(t2) :: y2
 end subroutine

 subroutine foo3 (x3,y3)
  type(t3),allocatable :: x3
  type(t3) :: y3
 end subroutine

 real function foo6 (a,b)
   real :: a,b
   foo6 = 1.
 end function

 integer function foo7 ()
   foo7 = 2
 end function

 character function foo8 (i)
   integer :: i
 end function

end module m
