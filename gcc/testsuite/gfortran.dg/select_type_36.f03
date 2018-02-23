! { dg-do run }
!
! Test the fix for PR69834 in which the two derived types below
! had the same hash value and so generated an error in the resolution
! of SELECT TYPE.
!
! Reported by James van Buskirk on clf:
! https://groups.google.com/forum/#!topic/comp.lang.fortran/0bm3E5xJpkM
!
module types
   implicit none
   type CS5SS
      integer x
      real y
   end type CS5SS
   type SQS3C
      logical u
      character(7) v
   end type SQS3C
   contains
      subroutine sub(x, switch)
         class(*), allocatable :: x
         integer :: switch
         select type(x)
            type is(CS5SS)
               if (switch .ne. 1) STOP 1
            type is(SQS3C)
               if (switch .ne. 2) STOP 2
            class default
               STOP 3
         end select
      end subroutine sub
end module types

program test
   use types
   implicit none
   class(*), allocatable :: u1, u2

   allocate(u1,source = CS5SS(2,1.414))
   allocate(u2,source = SQS3C(.TRUE.,'Message'))
   call sub(u1, 1)
   call sub(u2, 2)
end program test
