! { dg-do run }
!
! Test the fix for PR77703, in which calls of the pointer function
! caused an ICE in 'gfc_trans_auto_character_variable'.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
module m
   implicit none
   private
   integer, parameter, public :: n = 2
   integer, parameter :: ell = 6

   character(len=n*ell), target, public :: s

   public :: t
contains
   function t( idx ) result( substr )
      integer, intent(in) :: idx
      character(len=ell), pointer  :: substr

      if ( (idx < 0).or.(idx > n) ) then
         error stop
      end if
      substr => s((idx-1)*ell+1:idx*ell)
   end function t
end module m

program p
   use m, only : s, t, n
   integer :: i

   ! Define 's'
   s = "123456789012"

   ! Then perform operations involving 't'
   if (t(1) .ne. "123456") stop 1
   if (t(2) .ne. "789012") stop 2

   ! Do the pointer function assignments
   t(1) = "Hello "
   if (s .ne. "Hello 789012") Stop 3
   t(2) = "World!"
   if (s .ne. "Hello World!") Stop 4
end program p
