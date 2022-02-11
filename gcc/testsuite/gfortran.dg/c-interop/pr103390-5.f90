! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Check that copy loops to ensure contiguity of the result of a function
! that returns a pointer to an array are generated properly after fixing
! pr103390, and that it does not ICE.  This variant is for an intent(in)
! dummy argument so no copy-out is needed, only copy-in.

program p
   integer, pointer :: z(:)
   integer, target :: x(3) = [1, 2, 3]
   z => x
   call s(i(z))
contains
   function i(x)
      integer, pointer :: i(:)
      integer, pointer :: x(:)
      i => x
   end
   subroutine s(x) bind(c)
      integer, contiguous, intent(in) :: x(:)
   end
end

! Expect a copy loop before the call to S.  
! { dg-final { scan-tree-dump-times "while \\(1\\)" 1 "original" } }
