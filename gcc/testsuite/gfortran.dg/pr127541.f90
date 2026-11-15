! { dg-do compile }
!
! PR 127541
! PR 53653

subroutine s
  type t
    integer :: i
  end type
  class(t), allocatable :: x, z(:), b(:), v(:), w(:)
  allocate (z(1), source=[x])
  allocate (b, source=[x, x])
  print *, [v, w]
end subroutine
