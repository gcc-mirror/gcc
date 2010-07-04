! { dg-do compile }
!
! PR fortran/44556
!
! Contributed by Jonathan Hogg and Steve Kargl.
!
program oh_my
   implicit none
   type a
      integer, allocatable :: b(:), d(:)
      character(len=80) :: err
      character(len=80), allocatable :: str(:)
      integer :: src
   end type a

   integer j
   type(a) :: c
   c%err = 'ok'
   allocate(c%d(1)) 
   allocate(c%b(2), errmsg=c%err, stat=c%d(1)) ! OK
   deallocate(c%b, errmsg=c%err, stat=c%d(1))  ! OK
   allocate(c%b(2), errmsg=c%err, stat=c%b(1)) ! { dg-error "the same ALLOCATE statement" }
   deallocate(c%b, errmsg=c%err, stat=c%b(1))  ! { dg-error "the same DEALLOCATE statement" }
   allocate(c%str(2), errmsg=c%str(1), stat=j) ! { dg-error "the same ALLOCATE statement" }
   deallocate(c%str, errmsg=c%str(1), stat=j)  ! { dg-error "the same DEALLOCATE statement" }
end program oh_my
