! { dg-do run }
! Test derived type constructors for derived types containing arrays.
! PR16919
program der_array_1
  implicit none
  integer n
  integer m
  ! The 4 components here test known shape array, unknown shape array,
  ! multi-dimensional arrays and array pointers
  type t
    integer :: a(2)
    integer :: b(2)
    integer, dimension(2, 3) :: c
    integer, pointer, dimension(:) :: p
  end type
  type(t) :: v
  integer, dimension(2, 3) :: d
  integer, dimension(:), pointer :: e
  integer, dimension(2) :: f

  m = 2
  f = (/3, 4/)
  d = reshape ((/5, 6, 7, 8, 9, 10/), (/2, 3/));
  allocate (e(2))

  v = t((/1, 2/), reshape (f, (/m/)), d, e);
  if (any (v%a .ne. (/1, 2/)) .or. any (v%b .ne. (/3, 4/)) &
      .or. any (v%c .ne. d) .or. .not. associated (v%p, e)) &
    STOP 1

  deallocate(e)
end program

