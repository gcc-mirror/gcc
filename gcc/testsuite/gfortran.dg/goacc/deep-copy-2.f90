! { dg-do compile }

! Test of attach/detach with "acc data", two clauses at once.

program dtype
  implicit none
  integer, parameter :: n = 512
  type mytype
    integer, allocatable :: a(:)
  end type mytype
  integer i

  type(mytype) :: var

  allocate(var%a(1:n))

!$acc data copy(var) copy(var%a) ! { dg-error "Symbol .var. has mixed component and non-component accesses" }

!$acc data copy(var%a) copy(var) ! { dg-error "Symbol .var. has mixed component and non-component accesses" }

!$acc parallel loop
  do i = 1,n
    var%a(i) = i
  end do
!$acc end parallel loop

!$acc end data

!$acc end data

  do i = 1,n
    if (i .ne. var%a(i)) stop 1
  end do

  deallocate(var%a)

end program dtype
