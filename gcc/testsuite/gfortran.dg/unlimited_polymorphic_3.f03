! { dg-do run }
! { dg-additional-options "-fdump-tree-dse-details" }
!
! Check that pointer assignments allowed by F2003:C717
! work and check null initialization of CLASS(*) pointers.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
program main
  interface
    subroutine foo_bc(z)
      class(*), pointer, intent(in) :: z
    end subroutine foo_bc
    subroutine foo_sq(z)
      class(*), pointer, intent(in) :: z
    end subroutine foo_sq
  end interface
  type, bind(c) :: bc
    integer :: i
  end type bc
  type sq
    sequence
    integer :: k
  end type sq
  type(bc), target :: w
  type(sq), target :: x
  class(*), pointer :: y, z
  w%i = 23
  y => w
  z => y ! unlimited => unlimited allowed
  call foo_bc(z)
  x%k = 42
  y => x
  z => y ! unlimited => unlimited allowed
  call foo_sq(z)
  call bar
contains
  subroutine bar
    type t
    end type t
    type(t), pointer :: x
    class(*), pointer :: ptr1 => null() ! pointer initialization
    if (same_type_as (ptr1, x) .neqv. .FALSE.) STOP 1
  end subroutine bar

end program main

subroutine foo_bc(tgt)
  use iso_c_binding
  class(*), pointer, intent(in) :: tgt
  type, bind(c) :: bc
    integer (c_int) :: i
  end type bc
  type(bc), pointer :: ptr1
  ptr1 => tgt ! bind(c) => unlimited allowed
  if (ptr1%i .ne. 23) STOP 2
end subroutine foo_bc

subroutine foo_sq(tgt)
  class(*), pointer, intent(in) :: tgt
  type sq
    sequence
    integer :: k
  end type sq
  type(sq), pointer :: ptr2
  ptr2 => tgt ! sequence type => unlimited allowed
  if (ptr2%k .ne. 42) STOP 3
end subroutine foo_sq

! PR fortran/103662
! We used to produce multiple independant types for the unlimited polymorphic
! descriptors (types for class(*)) which caused stores to them to be seen as
! useless.
! { dg-final { scan-tree-dump-not "Deleted dead store: z._data = &w" "dse1" { target __OPTIMIZE__ } } }
! { dg-final { scan-tree-dump-not "Deleted dead store: z._data = &x" "dse1" { target __OPTIMIZE__ } } }
