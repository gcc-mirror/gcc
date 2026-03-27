! { dg-do run }
!
! Test the fix for PR114021 in which the ALLOCATE statement caused an ICE.
! The test checks that f() is called once per allocation, that the result
! of the allocation is correct and that a deep copy of w%x1 has been effected
! in 's2' without freeing it. 's3' is a variant, which produced no fewer than
! three calls to f() in the course of the assignment and the deep copy of
! the allocatable component.
!
! Contributed by Steve Kargl  <kargl@gcc.gnu.org>
!
module m1
   type y
      integer, allocatable:: x1(:)
   end type
   type(y), target :: w
   integer :: c = 0
contains
   function f()
      type(y), pointer :: f
      f => w
      c = c + 1
   end function
end

subroutine s1
   use m1
   type(y), allocatable :: x
   allocate(x, source = f())
   if ((c /= 1) .or. (allocated (x%x1))) stop 1
end

subroutine s2
   use m1
   type(y), pointer :: x
   allocate(x, source = f())
   if ((c /= 2) .or. (.not.allocated (x%x1))) stop 2
   if (any (abs (x%x1 - [3.0,4.0]) > 1e-6)) stop 3
   x%x1 = [5.0,6.0]
   if (allocated (x%x1)) deallocate (x%x1)
   if (associated (x)) deallocate (x)
end

subroutine s3
  use m1
  implicit none
  type(y), allocatable :: x
  allocate (x)
  x = f()
  if (any (abs (x%x1 - [3.0,4.0]) > 1e-6)) stop 4
end

   use m1
   call s1
   w%x1 = [1.0,2.0]
   if (c /= 1) stop 5
   w%x1 = [3.0,4.0]
   call s2
   if (c /= 2) stop 6
   call s3
   if (c /= 3) stop 7
   if (.not.allocated (w%x1) .or. any (abs (w%x1 - [3.0,4.0]) > 1e-6)) stop 8
   if (allocated (w%x1)) deallocate (w%x1)
end
