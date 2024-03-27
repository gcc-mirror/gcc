! { dg-do compile }
! PR fortran/114474 - DATA and derived types with pointer components

program pr114474
  implicit none
  integer, target     :: ii = 42    ! initial data target

  integer, target     :: jj = 24
  integer, pointer    :: qq => jj
  ! ii and jj resolve slightly differently when the data statement below
  ! is reached, as jj is resolved outside the structure constructor first

  type t
     integer, pointer :: h
  end type t

  integer, target     :: kk(7) =  23
  integer, pointer    :: ll(:) => kk

  type t1
     integer          :: m(7)
  end type t1

  type(t)             :: x1, x2, x3, x4, x5
  type(t), parameter  :: z1 = t(null())

  type(t1), target    :: tt = t1([1,2,3,4,5,6,7])
  type(t1), parameter :: vv = t1(22)
  type(t1)            :: w1, w2
  integer,  pointer   :: p1(:) => tt% m

  data x1 / t(null())  /
  data x2 / t(ii)      / ! ii is initial data target
  data x3 / t(jj)      / ! jj is resolved differently...
  data x4 / t(tt%m(3)) / ! pointer association with 3rd element

  data w1 / t1(12)     /
  data w2 / t1(vv%m)   /

  if (      associated (x1% h)) stop 1
  if (.not. associated (x2% h)) stop 2
  if (.not. associated (x3% h)) stop 3
  if (.not. associated (x4% h)) stop 4
  if (x2% h /= 42) stop 5
  if (x3% h /= 24) stop 6
  if (x4% h /=  3) stop 7
 
  if (any (w1%m /= 12  )) stop 8
  if (any (w2%m /= vv%m)) stop 9
end


subroutine sub
  implicit none

  interface
     real function myfun (x)
       real, intent(in) :: x
     end function myfun
  end interface

  type u
     procedure(myfun), pointer, nopass :: p
  end type u

  type(u)            :: u3 = u(null())
  type(u), parameter :: u4 = u(null())
  type(u)            :: u1, u2

  data u1 / u(null()) /
  data u2 / u(myfun)  /
end

real function myfun (x)
  real, intent(in) :: x
  myfun = x
end function myfun
