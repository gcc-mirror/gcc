! { dg-do run }
! PR fortran/121263 - fix TRANSFER with rank 1 unlimited polymorhpic
!
! Based on original testcase by Chris Cox.

module stdlib_hashmap_wrappers
  implicit none
contains
  subroutine set_rank_one_key_int( key, value )
    integer, allocatable, intent(inout) :: key(:)
    class(*), intent(in)                :: value(:)
    key = transfer( value, key )
  end subroutine

  subroutine set_rank_one_key_cx ( key, value )
    complex, allocatable, intent(inout) :: key(:)
    class(*), intent(in)                :: value(:)
    key = transfer( value, key )
  end subroutine

  subroutine set_first_key_int   ( key, value )
    integer, intent(inout) :: key
    class(*), intent(in)   :: value(:)
    key = transfer( value(1), key )
  end subroutine
end module

program p
  use stdlib_hashmap_wrappers
  implicit none
  integer, allocatable :: a(:), b(:)
  complex, allocatable :: c(:), d(:)
  class(*),allocatable :: z(:)
  integer :: m
  a = [1, 2, 3, 4, 5]
  c = cmplx (a, -a)
  call set_rank_one_key_int (b, a)
  call set_rank_one_key_cx  (d, c)
  call set_first_key_int    (m, a)
! print *, b
! print *, d
  if (size (a) /= size (b)) stop 1
  if (any  (a  /=       b)) stop 2
  if (size (c) /= size (d)) stop 3
  if (any  (c  /=       d)) stop 4
  if (m /= 1) stop 5
  deallocate (d)
  z = c
  d = transfer (z, d)
  if (size (c) /= size (d)) stop 6
  if (any  (c  /=       d)) stop 7
  deallocate (a, b, c, d, z)
end program p
