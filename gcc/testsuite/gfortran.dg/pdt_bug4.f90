! { dg-do run }
! { dg-options "-fcoarray=single" }
! Used to seg fault on exit due to attempted double free of pdt_array components.
! See attachment to PR82173 comment 10 - reproducers/bug4-dealloc-scope-exit.f90
program n1
  type :: t(n)
    integer, len :: n = 1
    integer :: v(n)
  end type
  type(t(4)), allocatable :: a[:]  ! Allocatable coarray
  type(t(3)), allocatable :: b     ! NON-deferred len, allocatable
  type(t(3)), allocatable :: c     !         ditto
  integer :: istat
  allocate (a[*])
  allocate (b)
  allocate (c)
  a%v = 1
  b%v = 2
  c%v = 3
  deallocate (a, b, c)
  deallocate (c, stat = istat) ! Deallocating already allocated caused segfault.
  print *, 'reached end', istat
end program ! Segmentation fault - invalid memory reference
