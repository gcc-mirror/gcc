! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 57071 - Check that 2**k is transformed into ishift(1,k).
program main
  implicit none
  integer :: i,m,v
  integer, parameter :: n=30
  integer, dimension(-n:n) :: a,b,c,d,e
  m = n

  v = 2
  ! Test scalar expressions.
  do i=-n,n
     if (2**i /= v**i) call abort
  end do

  ! Test array constructors
  b = [(2**i,i=-m,m)]
  c = [(v**i,i=-m,m)]
  if (any(b /= c)) call abort

  ! Test array expressions
  a = [(i,i=-m,m)]
  d = 2**a
  e = v**a
  if (any(d /= e)) call abort
end program main
! { dg-final { scan-tree-dump-times "_gfortran_pow_i4_i4" 3 "original" } }
