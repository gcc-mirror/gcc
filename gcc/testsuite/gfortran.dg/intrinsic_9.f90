! { dg-do run }
!
! PR fortran/92754
!
! Contributed by G. Steinmetz
!

program p
   integer :: max
   block
      character :: x = max('a','b')
      !print *, x
      if (x /= 'b') stop 1
   end block
end
