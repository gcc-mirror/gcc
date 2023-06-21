! { dg-do run }
!
! Contributed by Rich Townsend  <townsend@astro.wisc.edu>
!
program alloc_char_type
   implicit none
   integer, parameter :: start = 1, finish = 4
   character(3) :: check(4)
   type mytype
      character(:), allocatable :: c(:)
   end type mytype
   type(mytype) :: a
   type(mytype) :: b
   integer :: i
   a%c = ['foo','bar','biz','buz']
   check = ['foo','bar','biz','buz']
   b = a
   do i = 1, size(b%c)
      if (b%c(i) .ne. check(i)) stop 1
   end do
   if (any (a%c .ne. check)) stop 2
   if (any (a%c(start:finish) .ne. check)) stop 3
   deallocate (a%c)
   deallocate (b%c)
end
