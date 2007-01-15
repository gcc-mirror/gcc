! { dg-do compile }
! Tests the fix for PR28172, in which an ICE would result from
! the contained call with an alternate retrun.

! Contributed by Tobias Schlüter <tobi@gcc.gnu.org>

program blubb
  call otherini(*998)
  stop
998 stop
contains
 subroutine init
   call otherini(*999)
   return
999 stop
 end subroutine init
end program blubb
