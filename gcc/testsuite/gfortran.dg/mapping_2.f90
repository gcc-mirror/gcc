! { dg-do run }
! Tests the fix for PR33998, in which the chain of expressions
! determining the character length of my_string were not being
! resolved by the formal to actual mapping.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module test
   implicit none
   contains
      function my_string(x)
         integer i
         real, intent(in) :: x(:)
         character(0) h4(1:minval([(i,i=30,32), 15]))
         character(0) sv1(size(x,1):size(h4))
         character(0) sv2(2*lbound(sv1,1):size(h4))
         character(lbound(sv2,1)-3) my_string

         do i = 1, len(my_string)
            my_string(i:i) = achar(modulo(i-1,10)+iachar('0'))
         end do
      end function my_string
end module test

program len_test
   use test
   implicit none
   real x(7)

   if (my_string(x) .ne. "01234567890") call abort ()
end program len_test
