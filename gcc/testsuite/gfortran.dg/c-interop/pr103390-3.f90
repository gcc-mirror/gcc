! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of the result of a function
! that returns a non-pointer array are generated properly after fixing
! pr103390, and that it does not ICE.  In this case no copying is required.

program p
   integer, pointer :: z(:)
   integer, target :: x(3) = [1, 2, 3]
   z => x
   call s(i(z))
contains
   function i(x)
      integer :: i(3)
      integer, pointer :: x(:)
      i = x
   end
   subroutine s(x) bind(c)
      integer, contiguous :: x(:)
   end
end

! Expect one loop to copy the array contents to a temporary in function i.
! { dg-final { scan-tree-dump-times "while \\(1\\)" 1 "original" } }

! It should not emit code to check the contiguous property.
! { dg-final { scan-tree-dump-not "contiguous\\.\[0-9\]+" "original" } }

