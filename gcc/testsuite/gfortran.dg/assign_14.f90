! { dg-do compile }
! { dg-additional-options {-fdump-tree-original} }
!
! PR fortran/121185
! Check that an intermediary variable is used to reference component a.
! { dg-final { scan-tree-dump-not {->b->a} original } }

program p
  implicit none
  type t
     integer, allocatable :: a(:)
  end type t
  type u
     type(t), allocatable :: b
  end type u
  type v
     type(u), allocatable :: c
  end type v
  type(v) :: z
  z%c = u()
  z%c%b = t()
  z%c%b%a = [1,2]
  z%c%b%a = z%c%b%a * 2
end
