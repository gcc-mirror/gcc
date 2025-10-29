! { dg-do compile }
!
! Test the fix for PR122165.
!
! Contributed by Steve Kargl  <kargls@comcast.net>
!
program foo
   implicit none
   type dt(k,l)
      integer(8), len :: k = 1
      integer(8), KIND :: l = 1
      character(k) :: arr
   end type
   type(dt(:)), allocatable  :: d1
   if (d1%k%kind /= 8) stop 1 ! { dg-error "cannot be followed by the type inquiry ref" }
   if (d1%l%kind /= 8) stop 2 ! { dg-error "cannot be followed by the type inquiry ref" }
end
