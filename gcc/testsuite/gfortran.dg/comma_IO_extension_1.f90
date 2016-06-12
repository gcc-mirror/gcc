! { dg-do compile }
! PR 60751
! Contributed by Walter Spector <w6ws@earthlink.net>
program extracomma
  implicit none

  write (*,*), 1, 2, 3 ! { dg-warning "Legacy Extension: Comma before i/o item list" }
end program
