! { dg-do run }
! PR 71795 - wrong result when putting an array constructor
! instide an iterator.
     program test

     implicit none
     integer :: i,n
     logical, dimension(1) :: ra
     logical :: rs
     integer, allocatable :: a(:)

     allocate ( a(1) )

     n = 1
     a = 2

     ra = (/ (any(a(i).eq.(/1,2,3/)) ,i=1,n) /)
     if (.not. all(ra)) STOP 1
     rs = any ( (/ (any(a(i).eq.(/1,2,3/)) ,i=1,n) /) )
     if (.not. rs) STOP 2
   end program test
