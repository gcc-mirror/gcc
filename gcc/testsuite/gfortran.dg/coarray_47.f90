! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
program p
   type t
      integer, allocatable :: t
   end type
   type(t) :: x
   integer :: i = -1
   print *, transfer(i, x) ! { dg-error "cannot have ALLOCATABLE components" }
end
