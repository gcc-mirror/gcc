! { dg-do compile }
!
! Test case contributed by Mark Eggleston  <mark.eggleston@codethink.com>

program test
  integer(4) :: x
  
  x = x // "rubbish"       ! { dg-error "INTEGER\\(4\\)/CHARACTER\\(7\\)" }
  x = 4_"more rubbish" + 6 ! { dg-error "CHARACTER\\(12,4\\)/INTEGER\\(4\\)" }
end program
