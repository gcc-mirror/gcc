! { dg-do run }
!
! Test the fix for PR77325, which casued an ICE in the gimplifier. The
! segafults in 'contains_struct_check' were found while diagnosing the PR.
!
! Contributed by Gerhard Steinmetz  <gerhard.steinmetz.fortran@t-online.de>
!
program p
   character(3), parameter :: a(3) = ['abc', 'def', 'ghi']
   character(1), parameter :: c(3) = ['a', 'b', 'c']
   character(:), allocatable :: z(:)
   z = c([3,2])          ! Vector subscripts caused an iCE in the gimplifier.
   if (any (z .ne. ['c', 'b'])) stop 1
   z = c
   if (any (z .ne. ['a', 'b', 'c'])) stop 2
   z = c(2:1:-1)
   if (any (z .ne. ['b', 'a'])) stop 3
   z = c(3)
   if (any (z .ne. ['c', 'c'])) stop 4
   z = a([3,1,2])
   if (any (z .ne. ['ghi', 'abc', 'def'])) stop 5
   z = a(1:2)(2:3)       ! Substrings caused a segfault in 'contains_struct_check'.
   if (any (z .ne. ['bc', 'ef'])) stop 6
   z = a([2,3,1])(2:3)   ! ditto
   if (any (z .ne. ['ef', 'hi', 'bc'])) stop 7
   deallocate (z)
end
