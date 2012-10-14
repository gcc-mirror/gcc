! { dg-do run }
!
! PR 54784: [4.7/4.8 Regression] [OOP] wrong code in polymorphic allocation with SOURCE
!
! Contributed by Jeremy Kozdon <jkozdon@gmail.com>

program bug
  implicit none

  type :: block
    real, allocatable :: fields
  end type

  type :: list
    class(block),allocatable :: B
  end type

  type :: domain
    type(list),dimension(2) :: L
  end type

  type(domain) :: d
  type(block) :: b1

  allocate(b1%fields,source=5.)
  
  allocate(d%L(2)%B,source=b1)           ! wrong code
  
  if (d%L(2)%B%fields/=5.) call abort()

end program
