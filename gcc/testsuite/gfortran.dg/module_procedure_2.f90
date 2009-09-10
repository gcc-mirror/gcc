! { dg-do compile }
program test
   implicit none
   intrinsic sin
   interface gen2
      module procedure sin  ! { dg-error "cannot be a MODULE PROCEDURE" }
   end interface gen2
end program test
