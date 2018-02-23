! { dg-do compile }
!
! Tests the fix for PR82606.
!
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
program p
   type t(a, b)
      integer, len :: b   ! Note different order of component declarations
      integer, kind :: a  ! compared with the type_spec_list order.
      real(a) :: r(b)
   end type
   type(t(8, :)), allocatable :: x
   real(x%a) :: y         ! Used to die here because initializers were mixed up.
   allocate(t(8, 2) :: x)
   if (kind(y) .ne. x%a) STOP 1
   deallocate(x)
end
