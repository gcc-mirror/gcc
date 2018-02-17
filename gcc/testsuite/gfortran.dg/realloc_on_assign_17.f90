! { dg-do run }
! Test the fix for PR47517
!
! Reported by Tobias Burnus  <burnus@gcc.gnu.org>
! from a testcase by James Van Buskirk
module mytypes
   implicit none
   type label
      integer, allocatable :: parts(:)
   end type label
   type table
      type(label), allocatable :: headers(:)
   end type table
end module mytypes

program allocate_assign
   use mytypes
   implicit none
   integer, parameter :: ik8 = selected_int_kind(18)
   type(table) x1(2)
   type(table) x2(3)
   type(table), allocatable :: x(:)
   integer i, j, k
   integer(ik8) s
   call foo
   s = 0
   do k = 1, 10000
      x = x1
      s = s+x(2)%headers(2)%parts(2)
      x = x2
      s = s+x(2)%headers(2)%parts(2)
   end do
   if (s .ne. 40000) STOP 1
contains
!
! TODO - these assignments lose 1872 bytes on x86_64/FC17
! This is PR38319
!
   subroutine foo
       x1 = [table([(label([(j,j=1,3)]),i=1,3)]), &
             table([(label([(j,j=1,4)]),i=1,4)])]

       x2 = [table([(label([(j,j=1,4)]),i=1,4)]), &
             table([(label([(j,j=1,5)]),i=1,5)]), &
             table([(label([(j,j=1,6)]),i=1,6)])]
   end subroutine
end program allocate_assign
