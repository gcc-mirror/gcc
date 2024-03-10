! { dg-do compile }
! { dg-options "-fmax-array-constructor=65536 -fdump-tree-original" }
! PR fortran/103794

program p
  integer :: i, j
  integer, parameter :: a(2) = 2
  integer, parameter :: e(*) = [(reshape([1,2,3,4],  (a*i)), i=1,1)]
  integer, parameter :: f(*,*) = reshape([1,2,3,4], [(a*i,   i=1,1)])
  integer, parameter :: g(*,*) = reshape([([1,2,3,4],j=1,16383)],[(a*i,i=1,1)])
  integer, parameter :: s1(*) = &
       shape(reshape([([1,2,3,4],j=1,16383)],[(a*i,i=1,1)]))
  logical, parameter :: l1 = all (e == [1,2,3,4])
  logical, parameter :: l2 = all (f == reshape([1,2,3,4],[2,2]))
  logical, parameter :: l3 = size (s1) == 2 .and. all (s1 == 2)
  logical, parameter :: l4 = all (f == g)
  print *, e
  print *, f
  if (.not. l1) stop 1
  if (.not. l2) stop 2
  if (.not. l3) stop 3
  if (.not. l4) stop 4
  if (any (shape (reshape([1,2], [([2]*i, i=1,1)])) /= 2)) stop 5
  ! The following is compile-time simplified due to shape():
  print *, shape(reshape([([1,2,3,4],j=1,20000)],[(a*i,i=1,1)]))
  if (any (shape(reshape([([1,2,3,4],j=1,20000)],[(a*i,i=1,1)])) /= 2)) stop 6
  if (any (reshape([([1,2,3,4],j=1,16383)],[(a*i,i=1,1)]) /= f)) stop 7
  ! The following is not compile-time simplified:
  print *, reshape([([1,2,3,4],j=1,20000)],[(a*i,i=1,1)])
  if (any (reshape([([1,2,3,4],j=1,20000)],[(a*i,i=1,1)]) /= f)) stop 8
end

! { dg-final { scan-tree-dump-times "_gfortran_reshape_4" 2 "original" } }
! { dg-final { scan-tree-dump-times "_gfortran_stop_numeric" 1 "original" } }
