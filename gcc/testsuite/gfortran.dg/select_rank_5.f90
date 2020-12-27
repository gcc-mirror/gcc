! { dg-do run }
!
! Test the fixes for PR97723 and PR97694.
!
! Contributed by Martin  <mscfd@gmx.net>
!
module mod
   implicit none
   private
   public cssel

contains

function cssel(x) result(s)
   character(len=:), allocatable :: s
   class(*), dimension(..), optional, intent(in) :: x
   if (present(x)) then
      select rank (x)
      rank (0)
         s = '0' ! PR97723: ‘assign’ at (1) is not a function
                 ! PR97694: ICE in trans-stmt.c(trans_associate_var)
      rank (1)
         s = '1' ! PR97723: ‘assign’ at (1) is not a function
      rank default
         s = '?' ! PR97723: ‘assign’ at (1) is not a function
      end select
   else
      s = '-'
   end if
end function cssel

end module mod

program classstar_rank
   use mod
   implicit none

   integer :: x
   real, dimension(1:3) :: y
   logical, dimension(1:2,1:2) :: z

   if (any ([cssel(x),cssel(y),cssel(z),cssel()] .ne. ['0','1','?','-'])) stop 1

end program classstar_rank
