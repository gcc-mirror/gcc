! { dg-do compile }
!
! Contributed by Steve Kargl  <sgk@troutmask.apl.washington.edu>
!
program foo
   implicit none
   integer i
   i = 42
   if (i /= 42) stop 1
   call bah
   contains
      subroutine bah       ! { dg-error "is already defined at" }
         i = 43
         if (i /= 43) stop 2
      end subroutine bah
      subroutine bah       ! { dg-error "is already defined at" }
         ! import statement missing a comma
         import none       ! { dg-error "Unexpected IMPORT statement" }
         i = 44            ! { dg-error "Unexpected assignment" }
      end subroutine bah   ! { dg-error "Expecting END PROGRAM" }
end program foo
