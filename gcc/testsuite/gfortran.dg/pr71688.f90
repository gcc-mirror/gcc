! { dg-do compile }
! { dg-options "-fcoarray=lib" }

program p
   call s
contains
   subroutine s
      real :: x[*] = 1
      block
      end block
      x = 2
   end
end
