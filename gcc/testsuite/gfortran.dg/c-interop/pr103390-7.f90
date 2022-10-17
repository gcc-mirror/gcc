! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of the result of an array
! section expression are generated properly after fixing pr103390, and
! that it does not ICE.

program p
   integer, pointer :: z(:)
   integer :: A(5) = [1, 2, 3, 4, 5]
   call s(A(::2))
contains
   subroutine s(x) bind(c)
      integer, contiguous :: x(:)
   end
end

! Expect copy loops before and after the call to S.  
! { dg-final { scan-tree-dump-times "while \\(1\\)" 2 "original" } }
