! { dg-do compile }
! { dg-additional-options "-O0" }
!
! PR fortran/104228
! The code generated code for the program below wrongly pushed the Y character
! length variable to both P and S scope, which was leading to an ICE when
! address sanitizer was in effect

program p
   character(:), allocatable :: x(:)
   call s
contains
   subroutine s
      associate (y => x)
         y = [x]
      end associate
   end
end

