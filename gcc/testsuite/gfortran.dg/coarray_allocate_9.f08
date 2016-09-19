! { dg-do run }
! { dg-options "-fcoarray=lib -lcaf_single" }

! Contributed by Damian Rouson

program main
  implicit none

  type particles
    real x(2)
  end type

  type vector
    type(particles), allocatable :: v(:)
  end type

  type(vector) :: outbox[*]
  type(particles), allocatable :: object(:)[:]

  allocate(outbox%v(1), source=particles(this_image()))

  if (any( outbox[1]%v(1)%x(1:2) /= [ 1.0, 1.0] )) call abort()
  if (any( outbox[1]%v(1)%x(:) /= [ 1.0, 1.0] )) call abort()
  if (any( outbox[1]%v(1)%x /= [ 1.0, 1.0] )) call abort()

  allocate(object(1)[*], source=particles(this_image()))

  if (any( object(1)[1]%x(1:2) /= [ 1.0, 1.0] )) call abort()
  if (any( object(1)[1]%x(:) /= [ 1.0, 1.0] )) call abort()
  if (any( object(1)[1]%x /= [ 1.0, 1.0] )) call abort()
end program
