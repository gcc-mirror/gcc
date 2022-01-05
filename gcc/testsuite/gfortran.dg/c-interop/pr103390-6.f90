! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of transpose result are
! generated properly after fixing pr103390, and that it does not ICE.
! This variant is for an intent(in) dummy argument so no copy-out
! is needed, only copy-in.

program p
   integer, pointer :: z(:,:)
   integer, target :: x(3,3) = reshape ([1, 2, 3, 4, 5, 6, 7, 8, 9], shape(x))
   z => x
   call s(transpose(z))
contains
   subroutine s(x) bind(c)
      integer, contiguous, intent(in) :: x(:,:)
   end
end

! Expect 2 nested copy loops before the call to S.  
! { dg-final { scan-tree-dump-times "while \\(1\\)" 2 "original" } }

