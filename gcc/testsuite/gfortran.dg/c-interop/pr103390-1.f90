! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! This program used to ICE in gimplification on the call to S, because it
! was trying to copy out the array after the call to something that wasn't
! an lvalue.

program p
   integer, pointer :: z(:)
   integer, target :: x(3) = [1, 2, 3]
   z => x
   call s(shape(z))
contains
   subroutine s(x) bind(c)
      integer, contiguous :: x(:)
   end
end

! It should not emit any copy loops, just the loop for inlining SHAPE.
! { dg-final { scan-tree-dump-times "while \\(1\\)" 1 "original" } }

! It should not emit code to check the contiguous property.
! { dg-final { scan-tree-dump-not "contiguous\\.\[0-9\]+" "original" } }
