! { dg-do run }
! { dg-options "-std=f2003" }
! { dg-require-effective-target fortran_real_16 }
!
! PR fortran/51407
!
! Valid in F2008, but in F95/F2003:
! { dg-output "Expected INTEGER for item 1 in formatted transfer, got REAL" }
! { dg-shouldfail "Only F2003: BOZ edit with REAL" }
!
       real(kind=16) :: x
       character(len=32) :: str1
       x = 1.0_16 + 2.0_16**(-105)
       write (str1,'(z32)') x
       write (str1,'(z0)') x
       end
