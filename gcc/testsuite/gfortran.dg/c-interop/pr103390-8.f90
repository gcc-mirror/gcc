! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of the result of an array
! section expression are generated properly after fixing pr103390,
! and that it does not ICE.  This case is for an intent(in)
! dummy so no copy-out should occur, only copy-in.

program p
   integer, pointer :: z(:)
   integer, parameter :: A(5) = [1, 2, 3, 4, 5]
   call s(A(::2))
contains
   subroutine s(x) bind(c)
      integer, contiguous, intent(in) :: x(:)
   end
end

! Expect a copy loop before the call to S.  
! { dg-final { scan-tree-dump-times "while \\(1\\)" 1 "original" } }
