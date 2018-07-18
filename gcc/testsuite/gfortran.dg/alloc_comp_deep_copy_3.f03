! { dg-do run }
!
! PR fortran/67721
! Check that scalar to array assignments of derived type constructor
! deep copy the value when there are allocatable components.

program p
  implicit none

  type :: t1
    integer :: c1
  end type t1
  type :: t2
    type(t1), allocatable :: c2
  end type t2

  block
    type(t2) :: v(4)

    v = t2(t1(3))
    v(2)%c2%c1 =  7
    v(3)%c2%c1 = 11
    v(4)%c2%c1 = 13

    if (v(1)%c2%c1 /=  3) STOP 1
    if (v(2)%c2%c1 /=  7) STOP 2
    if (v(3)%c2%c1 /= 11) STOP 3
    if (v(4)%c2%c1 /= 13) STOP 4
  end block
end program p
