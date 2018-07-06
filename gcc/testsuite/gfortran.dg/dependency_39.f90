! { dg-do run }
! PR 45777 - component ref aliases when both are pointers
module m1
  type t1
     integer, dimension(:), allocatable :: data
  end type t1
contains
  subroutine s1(t,d)
    integer, dimension(:), pointer :: d
    type(t1), pointer :: t
    d(1:5)=t%data(3:7)
  end subroutine s1
  subroutine s2(d,t)
    integer, dimension(:), pointer :: d
    type(t1), pointer :: t
    t%data(3:7) = d(1:5)
  end subroutine s2
end module m1

program main
  use m1
  type(t1), pointer :: t
  integer, dimension(:), pointer :: d
  allocate(t)
  allocate(t%data(10))
  t%data=(/(i,i=1,10)/)
  d=>t%data(5:9)
  call s1(t,d)
  if (any(d.ne.(/3,4,5,6,7/))) STOP 1
  t%data=(/(i,i=1,10)/)
  d=>t%data(1:5)
  call s2(d,t)
  if (any(t%data.ne.(/1,2,1,2,3,4,5,8,9,10/))) STOP 1
  deallocate(t%data)
  deallocate(t)
end program main
