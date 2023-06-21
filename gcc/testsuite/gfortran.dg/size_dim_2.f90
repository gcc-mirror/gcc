! { dg-do compile }
! PR fortran/104350 - ICE with SIZE and bad DIM in initialization expression
! Contributed by G. Steinmetz

program p
  implicit none
  integer :: k
  integer, parameter :: x(2,3) = 42
  integer, parameter :: s(*) = [(size(x,dim=k),k=1,rank(x))]
  integer, parameter :: t(*) = [(size(x,dim=k),k=1,3)]   ! { dg-error "out of range" }
  integer, parameter :: u(*) = [(size(x,dim=k),k=0,3)]   ! { dg-error "out of range" }
  integer, parameter :: v = product(shape(x))
  integer, parameter :: w = product([(size(x,k),k=0,3)]) ! { dg-error "out of range" }
  print *,        ([(size(x,dim=k),k=1,rank(x))])
  print *,         [(size(x,dim=k),k=1,rank(x))]
  print *,         [(size(x,dim=k),k=0,rank(x))]
  print *, product([(size(x,dim=k),k=1,rank(x))])
  print *, product([(size(x,dim=k),k=0,rank(x))])
end
