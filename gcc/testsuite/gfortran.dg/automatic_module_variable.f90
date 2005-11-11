! { dg-do compile }
! Tests fix for PR15976
!
module sd
  integer, parameter :: n = 20
  integer :: i(n)
  integer :: j(m) ! { dg-error "cannot be automatic or assumed shape" }
  integer, pointer :: p(:)
  integer, allocatable :: q(:)
contains
  function init (x, l)
    integer :: x(l)
    integer :: init(l)
    init = x
  end function init
end module sd
