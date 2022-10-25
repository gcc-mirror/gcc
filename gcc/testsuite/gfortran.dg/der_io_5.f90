! { dg-do compile }
! PR fortran/100971 - ICE: Bad IO basetype (7)
! Contributed by G.Steinmetz

program p
  implicit none
  type t
  end type
  class(t), allocatable :: a, b(:)
  type(t)               :: x, y(1)
  integer               :: i
  allocate (a,b(1))
  print *, [a]            ! { dg-error "Data transfer element at .1. cannot be polymorphic" }
  print *, [(b(i),i=1,1)] ! { dg-error "Data transfer element at .1. cannot be polymorphic" }
  print *, [x]
  print *, [(y(i),i=1,1)]
end
