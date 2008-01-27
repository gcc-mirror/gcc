! { dg-do compile }
! PR34990 ICE in gfc_typenode_for_spec, at fortran/trans-types.c:842
! Test case that of the reporters.
module test 
   implicit none 
   contains 
      function my_string(x) 
         integer i 
         real, intent(in) :: x(:) 
         character(0) h4(1:minval([(1,i=1,0)],1)) 
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

   write(*,*) my_string(x) 
end program len_test
