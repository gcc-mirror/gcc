! { dg-do run }
! Test the fix for PR47519, in which the character length was not
! calculated for the SOURCE expressions below and an ICE resulted. 
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
program note7_35
   implicit none
   character(:), allocatable :: name
   character(:), allocatable :: src
   integer n
   n = 10
   allocate(name, SOURCE=repeat('x',n))
   if (name .ne. 'xxxxxxxxxx') STOP 1
   if (len (name) .ne. 10 ) STOP 2
   deallocate(name)
   src = 'xyxy'
   allocate(name, SOURCE=repeat(src,n))
   if (name(37:40) .ne. 'xyxy') STOP 3
   if (len (name) .ne. 40 ) STOP 4
end program note7_35
