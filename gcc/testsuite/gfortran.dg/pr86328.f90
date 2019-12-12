! { dg-do run }
!
! Test the fix for PR86328 in which temporaries were not being
! assigned for array component references.
!
! Contributed by Martin  <mscfd@gmx.net>
!
program ptr_alloc

   type :: t
      class(*), allocatable :: val
   end type

   type :: list
      type(t), dimension(:), pointer :: ll
   end type

   integer :: i
   type(list) :: a

   allocate(a%ll(1:2))
   do i = 1,2
      allocate(a%ll(i)%val, source=i)
   end do

   do i = 1,2
     call rrr(a, i)
   end do

   do i = 1,2
      deallocate(a%ll(i)%val)
   end do
   deallocate (a%ll)
contains

   subroutine rrr(a, i)
      type(list), intent(in) :: a
      class(*), allocatable :: c
      integer :: i

      allocate(c, source=a%ll(i)%val)
      select type (c)
        type is (integer)
          if (c .ne. i) stop 1
      end select

   end subroutine

end
