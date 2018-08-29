! { dg-do run }
! { dg-options "-ffrontend-optimize -fdump-tree-original" }
! PR 57071 - Check that (-1)**k is transformed into 1-2*iand(k,1).
program main
  implicit none
  integer, parameter :: n = 3
  integer(kind=8), dimension(-n:n) :: a, b
  integer, dimension(-n:n) :: c, d, e
  integer :: m
  integer :: i, v
  integer (kind=2) :: i2

  m = n
  v = -1
  ! Test in scalar expressions
  do i=-n,n
     if (v**i /= (-1)**i) STOP 1
  end do

  ! Test in array constructors
  a(-m:m) = [ ((-1)**i, i= -m, m) ]
  b(-m:m) = [ (   v**i, i= -m, m) ]
  if (any(a .ne. b)) STOP 2

  ! Test in array expressions
  c = [ ( i, i = -n , n ) ]
  d = (-1)**c
  e = v**c
  if (any(d .ne. e)) STOP 3

  ! Test in different kind expressions
  do i2=-n,n
     if (v**i2 /= (-1)**i2) STOP 4
  end do

end program main
! { dg-final { scan-tree-dump-times "_gfortran_pow_i4_i4" 4 "original" } }
