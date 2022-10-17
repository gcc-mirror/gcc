! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of the result of an elemental
! array-valued expression are generated properly after fixing pr103390,
! and that it does not ICE.

program p
   integer, pointer :: z(:)
   integer :: a(3) = [1, 2, 3];
   integer :: b(3) = [4, 5, 6];
   call s(a + b);
contains
   subroutine s(x) bind(c)
      integer, contiguous :: x(:)
   end
end

! We only expect one loop before the call, to fill in the contiguous
! temporary.  No copy-out is needed since the temporary is effectively
! an rvalue.
! { dg-final { scan-tree-dump-times "while \\(1\\)" 1 "original" } }

! It should not emit code to check the contiguous property.
! { dg-final { scan-tree-dump-not "contiguous\\.\[0-9\]+" "original" } }

