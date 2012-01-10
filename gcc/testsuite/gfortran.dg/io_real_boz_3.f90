! { dg-do  run }
! { dg-options "-std=f2008" }
! { dg-require-effective-target fortran_real_16 }
!
! PR fortran/51407
!
! Fortran 2008 allows BOZ edit descriptors for real/complex.
!
       real(kind=4) :: x
       complex(kind=4) :: z
       character(len=64) :: str1

       x = 1.0_16 + 2.0_16**(-105)
       z = cmplx (1.0, 2.0)

       write (str1,'(b32)') x
       read (str1,'(b32)') x
       write (str1,'(o32)') x
       read (str1,'(o32)') x
       write (str1,'(z32)') x
       read (str1,'(z32)') x
       write (str1,'(b0)') x
       write (str1,'(o0)') x
       write (str1,'(z0)') x

       write (str1,'(2b32)') z
       read (str1,'(2b32)') z
       write (str1,'(2o32)') z
       read (str1,'(2o32)') z
       write (str1,'(2z32)') z
       read (str1,'(2z32)') z
       write (str1,'(2b0)') z
       write (str1,'(2o0)') z
       write (str1,'(2z0)') z
       end
