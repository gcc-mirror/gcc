! { dg-do compile }
!
! ALLOCATE statements with derived type specification
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

 type :: t1
  integer :: i
 end type

 type, extends(t1) :: t2
  real :: r
 end type

 type, extends(t2) :: t3
  real :: q
 end type

 type, abstract :: u0
  logical :: nothing
 end type

 type :: v1
  real :: r
 end type

 class(t1),dimension(:),allocatable :: x
 type(t2),dimension(:),allocatable :: y
 class(t3),dimension(:),allocatable :: z

 allocate(      x(1))
 allocate(t1 :: x(2))
 allocate(t2 :: x(3))
 allocate(t3 :: x(4))
 allocate(tx :: x(5))  ! { dg-error "Error in type-spec at" }
 allocate(u0 :: x(6))  ! { dg-error "may not be ABSTRACT" }
 allocate(v1 :: x(7))  ! { dg-error "is type incompatible with typespec" }

 allocate(      y(1))
 allocate(t1 :: y(2))  ! { dg-error "is type incompatible with typespec" }
 allocate(t2 :: y(3))
 allocate(t3 :: y(3))  ! { dg-error "is type incompatible with typespec" }

 allocate(      z(1))
 allocate(t1 :: z(2))  ! { dg-error "is type incompatible with typespec" }
 allocate(t2 :: z(3))  ! { dg-error "is type incompatible with typespec" }
 allocate(t3 :: z(4))

end

