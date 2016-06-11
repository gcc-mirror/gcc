! { dg-do compile }
! { dg-options "-std=legacy" }
! PR 60751
! Contributed by Walter Spector <w6ws@earthlink.net>
program extracomma
  implicit none

  write (*,*), 1, 2, 3
end program
