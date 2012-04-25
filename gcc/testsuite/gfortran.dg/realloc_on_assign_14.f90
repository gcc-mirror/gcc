! { dg-do compile }
! { dg-options "-Wrealloc-lhs-all -Wrealloc-lhs" }
!
! PR fortran/52196
!
implicit none
type t
  integer :: x
end type t
integer, allocatable :: a(:), b
real, allocatable :: r(:)
type(t), allocatable :: c(:)
character(len=:), allocatable :: str
character(len=:), allocatable :: astr(:)

allocate(a(2), b, c(1))
b = 4          ! { dg-warning "Code for reallocating the allocatable variable" }
a = [b,b]      ! { dg-warning "Code for reallocating the allocatable array" }
c = [t(4)]     ! { dg-warning "Code for reallocating the allocatable variable" }
a = 5          ! no realloc
c = t(5)       ! no realloc
str = 'abc'    ! { dg-warning "Code for reallocating the allocatable variable" }
astr = 'abc'   ! no realloc
astr = ['abc'] ! { dg-warning "Code for reallocating the allocatable array" }
a = reshape(a,shape(a)) ! { dg-warning "Code for reallocating the allocatable array" }
r = sin(r)     ! { dg-warning "Code for reallocating the allocatable array" }
r = sin(r(1))  ! no realloc
b = sin(r(1))  ! { dg-warning "Code for reallocating the allocatable variable" }

a = nar() ! { dg-warning "Code for reallocating the allocatable array" }
a = nar2() ! { dg-warning "Code for reallocating the allocatable array" }
contains
  function nar()
    integer,allocatable :: nar(:)
  end function
  function nar2()
    integer :: nar2(8)
  end function
end
