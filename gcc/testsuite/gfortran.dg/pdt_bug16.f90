! { dg-do run }
! ALLOCATE with MOLD= did not propagate the LEN parameter; SOURCE= does.
! See attachment to PR82173 comment 10 - ~/reducers/bug16-mold-loses-len.f90
program p
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
  end type
  type(t(:)), allocatable :: viaSource, viaMold
  type(t(5)) :: src1
  type(t(7)) :: src2
  src1%v = 1
  src2%v = 2
  allocate (viaSource, source=src1)
  allocate (viaMold, mold=src1)
  if (viaSource%n /= viaMold%n) stop 1
  if (size (viaSource%v, 1) /= size (viaMold%v, 1)) stop 2
!
! Do it again with a different value for n
!
  deallocate (viaSource, viaMold)
  allocate (viaSource, source=src2)
  allocate (viaMold, mold=src2)
  viaMold%v = 2
  if (viaSource%n /= 7) stop 3
  if (viaSource%n /= viaMold%n) stop 4
  if (size (viaMold%v, 1) /= 7) stop 5
  if (size (viaSource%v, 1) /= size (viaMold%v, 1)) stop 6
  if (sum (viaSource%v) /= sum (viaMold%v)) stop 7
  deallocate (viaSource, viaMold)
end program
