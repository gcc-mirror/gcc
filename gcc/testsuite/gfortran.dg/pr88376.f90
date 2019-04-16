! { dg-do compile }
module m
   integer :: n
contains
   subroutine s
      character(n(3)) :: c  ! { dg-error "not a function" }
   end
end
