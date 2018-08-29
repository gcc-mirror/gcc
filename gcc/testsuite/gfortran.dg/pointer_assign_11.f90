! { dg-do run }
!
! PR fortran/57530
!
!
! CLASS => CLASS pointer assignment for function results
!
module m
  implicit none
  type t
    integer :: ii = 55
  end type t
  type, extends(t) :: t2
  end type t2
contains
  function f1()
    class(t), pointer :: f1
    allocate (f1)
    f1%ii = 123
  end function f1
  function f2()
    class(t), pointer :: f2(:)
    allocate (f2(3))
    f2(:)%ii = [-11,-22,-33]
  end function f2
end module m

program test
  use m
  implicit none
  class(t), pointer :: p1, p2(:), p3(:,:)
  type(t) :: my_t
  type(t2) :: my_t2

  allocate (t2 :: p1, p2(1), p3(1,1))
  if (.not. same_type_as (p1, my_t2)) STOP 1
  if (.not. same_type_as (p2, my_t2)) STOP 2
  if (.not. same_type_as (p3, my_t2)) STOP 3

  p1 => f1()
  if (p1%ii /= 123) STOP 4
  if (.not. same_type_as (p1, my_t)) STOP 5

  p2 => f2()
  if (any (p2%ii /= [-11,-22,-33])) STOP 6
  if (.not. same_type_as (p2, my_t)) STOP 7

  p3(2:2,1:3) => f2()
  if (any (p3(2,:)%ii /= [-11,-22,-33])) STOP 8
  if (.not. same_type_as (p3, my_t)) STOP 9
end program test
