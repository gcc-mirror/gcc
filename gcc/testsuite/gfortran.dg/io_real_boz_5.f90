! { dg-do run }
! { dg-options "-std=f2008" }
! { dg-require-effective-target fortran_real_16 }
!
! PR fortran/51407
!
! Invalid in F2008 (accepted with -std=gnu)
! { dg-output "Expected numeric type for item 1 in formatted transfer, got CHARACTER" }
! { dg-shouldfail "Character type in BOZ" }
!
       character(len=32) :: str1
       x = 1.0_16 + 2.0_16**(-105)
       write (str1,'(z0)') 'X'
       end
