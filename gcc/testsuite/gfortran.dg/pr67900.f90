! { dg-do compile }
! PR fortran/67900
! Original code contributed by Giorgian Borca-Tasciuc
! giorgianb at gmail dot com
! 
program main
   implicit none
   interface f
      function f_real(x)
         real, bind(c) :: x
         real :: f_real
      end function f_real

      function f_integer(x)
         integer, bind(c) :: x
         integer :: f_integer
      end function f_integer
   end interface f
end program main
