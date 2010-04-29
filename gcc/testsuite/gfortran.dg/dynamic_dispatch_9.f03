! { dg-do run }
!
! [OOP] Ensure that different specifc interfaces are
! handled properly by dynamic dispatch.
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>
!
module m

 type :: t
 contains
  procedure :: a
  generic :: gen => a
 end type

 type,extends(t) :: t2
 contains
  procedure :: b
  generic :: gen => b
 end type

contains

  real function a(ct,x)
    class(t) :: ct
    real :: x
    a=2*x
  end function

  integer function b(ct,x)
    class(t2) :: ct
    integer :: x
    b=3*x
  end function

end


 use m
 class(t), allocatable :: o1
 type (t) :: t1
 class(t2), allocatable :: o2

 allocate(o1)
 allocate(o2)

 if (t1%gen(2.0) .ne. o1%gen(2.0)) call abort
 if (t1%gen(2.0) .ne. o2%gen(2.0)) call abort
 if (o2%gen(3) .ne. 9) call abort

end

! { dg-final { cleanup-modules "m" } }

