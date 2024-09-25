! { dg-do run }

! Check for PR fortran/82904
! Contributed by G.Steinmetz  <gscfq@t-online.de>

! This test checks that 'IPA pass: inline' passes.
! The initial version of the testcase contained coarrays, which does not work
! yet.

program p
   save
   character(:), allocatable :: x
   character(:), allocatable :: y
   allocate (character(3) :: y)
   allocate (x, source='abc')
   y = x

   if (y /= 'abc') stop 1
end

