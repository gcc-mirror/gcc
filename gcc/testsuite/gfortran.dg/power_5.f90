! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 57071 - Check that 1**k is transformed into 1
program main
  implicit none
  integer, parameter :: n = 3
  integer(kind=8), dimension(-n:n) :: a
  integer, dimension(-n:n) :: c, d
  integer :: m
  integer :: i, v
  integer (kind=2) :: i2

  v = 1
  m = n
  ! Test in scalar expressions
  do i=-n,n
     if (v /= 1**i) call abort
  end do

  ! Test in array constructors
  a(-m:m) = [ (1**i, i= -m, m) ]
  if (any(a .ne. v)) call abort

  ! Test in array expressions
  c = [ ( i, i = -n , n ) ]
  d = 1**c
  if (any(d .ne. v)) call abort

  ! Test in different kind expressions
  do i2=-n,n
     if (v /= 1**i2) call abort
  end do

end program main
! { dg-final { scan-tree-dump-times "_gfortran_pow_i4_i4" 0 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
