! { dg-do run }
!
! Test the correct processing of public generic statements and verify that they
! behave in the same way as public interfaces.
!
! Contributed by Steven Kargl  <kargls@comcast.net>
!
module foo

   implicit none

   private
   public bak1, bak2


   generic :: bak1 => bar, bah

   ! Should be equivalent to above.

   interface bak2
      module procedure bar
      module procedure bah
   end interface bak2


   contains
      function bar(i)
         real bar
         integer, intent(in) :: i
         bar = i
      end function bar
      function bah(x)
         real bah
         real, intent(in) :: x
         bah = x
      end function bah
end module foo

program snooze
   use foo
   if (bak1(42) /= bak2(42)) stop 1
   if (bak1(43.5) /= bak2(43.5)) stop 2 
end program snooze
