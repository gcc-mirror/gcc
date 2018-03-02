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
   print *, transfer(1, x) ! { dg-error "cannot have ALLOCATABLE components" }
end
