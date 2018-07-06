! { dg-do run }
! { dg-options "-fcoarray=single" }
!
! Tests the fix for the regression PR83901.
!
! Contributed by G Steinmetz  <gscfq@t-online.de>
!
program p
   character(8), allocatable :: x[:]
   allocate (x[*])
   x = 'abc'
   associate (y => x)
     if (y .ne. 'abc') stop 1
   end associate
end
