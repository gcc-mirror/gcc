! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of transpose result are
! still generated after fixing pr103390, and that it does not ICE.

program p
   integer, pointer :: z(:,:)
   integer, target :: x(3,3) = reshape ([1, 2, 3, 4, 5, 6, 7, 8, 9], shape(x))
   z => x
   call s(transpose(z))
contains
   subroutine s(x) bind(c)
      integer, contiguous :: x(:,:)
   end
end

! Expect 2 nested copy loops both before and after the call to S.  
! { dg-final { scan-tree-dump-times "while \\(1\\)" 4 "original" } }

