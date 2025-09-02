! { dg-do run }
!
! PR fortran/121342
! The polymorphic function result as actual argument used to force the loop
! bounds around the elemental call, altering access to the other arrays.

program p
  implicit none
  type :: t
    integer :: i
  end type
  type :: u
    integer :: i, a
  end type
  type(u) :: accum(5)
  integer :: a(3:7), k
  a = [ (k*k, k=1,5) ]
  call s(accum, f(), a)
  ! print *, accum%i
  ! print *, accum%a
  if (any(accum%i /= accum%a)) error stop 1
contains
  elemental subroutine s(l, c, a)
    type(u)  , intent(out) :: l
    class(t) , intent(in)  :: c
    integer  , intent(in)  :: a
    l%i = c%i
    l%a = a
  end subroutine
  function f()
    class(t), allocatable :: f(:)
    allocate(f(-1:3))
    f%i = [ (k*k, k=1,5) ]
  end function
end program
