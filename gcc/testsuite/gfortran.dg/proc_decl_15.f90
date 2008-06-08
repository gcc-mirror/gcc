! { dg-do run }
! PR fortran/35830
!
function f()
  real, allocatable :: f(:)
  allocate(f(1:3))
  f(1:3)= (/9,8,7/)
end function

program test
  implicit none
  abstract interface
    function ai()
      real, allocatable :: ai(:)
    end function
  end interface
  procedure(ai) :: f
  if(any(f() /= [9,8,7])) call abort()
  if(size(f()) /= 3) call abort()
end
