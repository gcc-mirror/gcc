! { dg-do run }
!
! PR fortran/42597
!
! Contributed by mrestelli@gmail.com
!

module mod_a
 implicit none

 abstract interface
  pure function intf(x) result(y)
   real, intent(in) :: x(:,:)
   real :: y(size(x,1),size(x,1),size(x,2))
  end function intf
 end interface

 procedure(intf), pointer :: p_fun => null()
end module mod_a

program main
  use mod_a
  implicit none

  procedure(intf), pointer :: p_fun2 => null()

  if (associated(p_fun) .or. associated(p_fun2)) &
    STOP 1
end program main
