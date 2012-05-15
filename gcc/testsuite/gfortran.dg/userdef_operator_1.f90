! { dg-do compile }
! Testcase from PR 25396: User defined operators returning arrays.
module geometry

  implicit none

  interface operator(.cross.)
     module procedure cross
  end interface

contains

    ! Cross product between two 3d vectors.
    pure function cross(a, b)
      real, dimension(3), intent(in) :: a,b
      real, dimension(3) :: cross

     cross = (/ a(2) * b(3) - a(3) * b(2), &
           a(3) * b(1) - a(1) * b(3), &
           a(1) * b(2) - a(2) * b(1) /)
    end function cross

end module geometry

program opshape
  use geometry

  implicit none

  real :: t(3,3), a

  a = dot_product (t(:,1), t(:,2) .cross. t(:,3))

end program opshape
