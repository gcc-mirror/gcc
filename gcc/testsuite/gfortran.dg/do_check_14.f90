! { dg-do compile }
! PR fortran/96469 - make sure that all legal variants pass.

module x
  implicit none
contains
  subroutine sub_intent_in(i)
    integer, intent(in) :: i
  end subroutine sub_intent_in
  subroutine sub_intent_unspec(i)
    integer :: i
  end subroutine sub_intent_unspec
  integer function fcn_intent_in(i)
    integer, intent(in) :: i
    fcn_intent_in = i + 42
  end function fcn_intent_in
  integer function fcn_intent_unspec (i)
    integer :: i
    fcn_intent_unspec = i + 42
  end function fcn_intent_unspec
end module x

program main
  use x
  implicit none
  integer :: i1, i2, i3, i4
  integer :: k, l
  do i1=1,10
     call sub1
  end do
  do i2=1,10
     call sub2
  end do
  do i3 = 1,10
     k = fcn3()
  end do
  do i4=1,10
     l = fcn4()
  end do
contains
  subroutine sub1
    call sub_intent_in (i1)
  end subroutine sub1
  subroutine sub2
    integer :: m
    m = fcn_intent_in (i2)
    print *,m
  end subroutine sub2
  integer function fcn3()
    call sub_intent_unspec (i3)
    fcn3 = 42
  end function fcn3
  integer function fcn4()
    fcn4 = fcn_intent_unspec (i4)
  end function fcn4
end program main
