! { dg-do run }
! Rejected valid code: `a` has a deferred type parameter, but `a(i)%extra`
! is an ordinary allocatable INTEGER array with deferred SHAPE and no type
! parameters at all. Therefore C937 does not apply to it.
! See attachment to PR82173 comment 10 - ~/reducers/bug14-bogus-deferred-param-error.f90
program p
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
    integer, allocatable :: extra(:)
  end type
  type(t(:)), allocatable :: a(:)
  allocate (t(4) :: a(3))
  allocate (a(1)%extra(2))    ! Error: Allocate-object at (1) with a deferred type parameter
  a(1)%extra = 5              ! requires either a type-spec or SOURCE tag or a MOLD tag
  if (sum (a(1)%extra) /= 10) stop 1
end program
