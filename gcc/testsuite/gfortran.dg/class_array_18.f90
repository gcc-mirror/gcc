! { dg-do compile }
!
! PR fortran/57535
!
program test
  implicit none
  type t
    integer :: ii = 55
  end type t
contains
  function func2()
    class(t), allocatable :: func2(:)
    allocate(func2(3))
    func2%ii = [111,222,333]
  end function func2
end program test
