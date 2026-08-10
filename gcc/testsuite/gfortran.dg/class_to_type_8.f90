! { dg-do run }
! PR fortran/53800

! A component of an array of extended derived types, passed to a TARGET
! dummy, is addressed through the span of its descriptor.  This must also
! hold for a section of such a dummy and for a subobject reference applied
! to it.
!
! Contributed by Mikael Morin  <mikael@gcc.gnu.org>

program p
  implicit none
  type :: t
    integer :: c1, c2
  end type
  type, extends(t) :: u
    integer :: c3
  end type
  type, extends(u) :: v
    integer :: c4
  end type
  type(v), target :: x(12)
  integer :: i
  x = [(v(i,i*i,i,i), i=1,size(x))]
  call s1(x(2::3)%c2, 1)
  call s2(x%u)
contains
  subroutine s1(a, error_idx)
    integer, intent(in) :: error_idx
    integer, target :: a(:)
    if (any(a /= [4, 25, 64, 121])) error stop error_idx * 10 + 1
  end subroutine
  subroutine s2(a)
    type(u), target :: a(:)
    if (any(a%c2 /= [(i*i, i=1,12)])) error stop 2
    if (any(a(2::3)%c2 /= [4, 25, 64, 121])) error stop 3
    call s1(a(2::3)%c2, 2)
    call s3(a(2::3)%c2)
    if (any(x(2::3)%c2 /= [-4, -25, -64, -121])) error stop 5
    x = [(v(i,i*i,i,i), i=1,size(x))]
  end subroutine
  subroutine s3(a)
    integer :: a(:)          ! copy-in/copy-out
    if (any(a /= [4, 25, 64, 121])) error stop 4
    a = -a
  end subroutine
end program
