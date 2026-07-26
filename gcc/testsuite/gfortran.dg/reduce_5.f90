! { dg-do run }
! { dg-shouldfail "foo" }

  implicit none
  integer :: a(3)
  logical :: m(2)
  a = 1
  m = .true.
  print *, get_reduce (a, m)
contains
  function get_reduce (array, mask) result (res)
    integer, intent(in) :: array(:)
    logical, intent(in) :: mask(:)
    integer :: res
    res = reduce (array, add, mask = mask, identity = 0)
  end function get_reduce
  pure function add (i, j) result (ij)
    integer, intent(in) :: i, j
    integer :: ij
    ij = i + j
  end function add
end
! { dg-output "shape mismatch between ARRAY and MASK in the REDUCE intrinsic \\(3/2\\)" }
