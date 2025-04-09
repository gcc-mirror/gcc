! { dg-do run }
!
! PR119540 comment2: REDUCE was getting the shape wrong. This testcase also
! verifies that the longest possible name for the OPERATION wrapper function
! is catered for.
!
! Contributed by Harald Anlauf  <anlauf@gcc.gnu.org>
!
program p2345678901234567890123456789012345678901234567890123456789_123
  implicit none
  integer, parameter :: n = 3
  integer, parameter :: vec(n) = [2, 5, 10]
  integer, parameter :: mat(n,2) = reshape([vec,2*vec],[n,2])
  integer :: mat_shape(2), reduce_shape(1), r
  integer, dimension(:), allocatable :: res1

  mat_shape = shape (mat)
  reduce_shape = shape (reduce (mat, add, 1), 1)
  if (reduce_shape(1) /= mat_shape(2)) stop 1

  reduce_shape = shape (reduce (mat, add, 1), 1)
  if (reduce_shape(1) /= mat_shape(2)) stop 2

  res1 = reduce (mat, add, 1)
  if (any (res1 /= [17, 34])) stop 3

  res1 = reduce (mat, add, 2)
  if (any (res1 /= [6, 15, 30])) stop 4

  r = reduce (vec, &
              o2345678901234567890123456789012345678901234567890123456789_123)
  if (r /= 17) stop 5

  deallocate (res1)
contains
  pure function add(i,j) result(sum_ij)
    integer, intent(in) :: i, j
    integer             :: sum_ij
    sum_ij = i + j
  end function add

  pure function o2345678901234567890123456789012345678901234567890123456789_123 (i, j) &
       result (sum)
    integer, intent(in) :: i, j
    integer             :: sum
    sum = i + j
  end function
end
