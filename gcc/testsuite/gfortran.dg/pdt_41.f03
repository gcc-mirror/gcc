! { dg-do run }
!
! Test the fix for pr99709 in which the object being passed to a PDT dummy
! with the value attribute was not a deep copy.
!
! Contribute by Xiao Liu  <xiao.liu@compiler-dev.com>
!
program value_f2008
  implicit none
  type :: matrix(k)
    integer, len :: k
    integer :: elements(k, k)
    !integer :: elements(2, 2)
  end type matrix

  type, extends(matrix) :: child
  end type child

  integer, parameter :: array_parm(2, 2) = reshape([1, 2, 3, 4], [2, 2])

  type(child(2)) :: obj
  obj%elements = array_parm

  call test_value_attr(2, obj)
  if (any (obj%elements /= array_parm)) stop 1 

  call test(2, obj)
  if (any (obj%elements /= 0)) stop 2 

contains

  subroutine test(n,  nonconstant_length_object)
    integer :: n
    type(child(n)) :: nonconstant_length_object
    if (nonconstant_length_object%k /= 2) stop 3
    if (any (nonconstant_length_object%elements /= array_parm)) stop 4
    nonconstant_length_object%elements = 0
  end subroutine test

  subroutine test_value_attr(n,  nonconstant_length_object)
    integer :: n
    type(child(n)), value :: nonconstant_length_object
    if (nonconstant_length_object%k /= 2) stop 5
    if (any (nonconstant_length_object%elements /= array_parm)) stop 6
    nonconstant_length_object%elements = 0
  end subroutine test_value_attr
end program value_f2008
