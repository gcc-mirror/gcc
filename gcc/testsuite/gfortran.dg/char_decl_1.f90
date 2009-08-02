! { dg-do compile }
! { dg-options "-std=legacy" }
!
! PR32644 "CHARACTER*1, c" produces "Unclassifiable statement"
program f
character*1, c
end program f
