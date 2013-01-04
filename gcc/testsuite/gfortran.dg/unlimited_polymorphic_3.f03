! { dg-do run }
!
! Check that pointer assignments allowed by F2003:C717
! work and check null initialization of CLASS(*) pointers.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
program main
  interface
    subroutine foo(z)
      class(*), pointer, intent(in) :: z
    end subroutine foo
  end interface
  type sq
    sequence
    integer :: i
  end type sq
  type(sq), target :: x
  class(*), pointer :: y, z
  x%i = 42
  y => x
  z => y ! unlimited => unlimited allowed
  call foo (z)
  call bar
contains
  subroutine bar
    type t
    end type t
    type(t), pointer :: x
    class(*), pointer :: ptr1 => null() ! pointer initialization
    if (same_type_as (ptr1, x) .neqv. .FALSE.) call abort
  end subroutine bar

end program main


subroutine foo(tgt)
  use iso_c_binding
  class(*), pointer, intent(in) :: tgt
  type, bind(c) :: s
    integer (c_int) :: k
  end type s
  type t
    sequence
    integer :: k
  end type t
  type(s), pointer :: ptr1
  type(t), pointer :: ptr2
  ptr1 => tgt ! bind(c) => unlimited allowed
  if (ptr1%k .ne. 42) call abort
  ptr2 => tgt ! sequence type => unlimited allowed
  if (ptr2%k .ne. 42) call abort
end subroutine foo
