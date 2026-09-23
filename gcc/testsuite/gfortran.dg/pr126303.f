! PR fortran/126303
! { dg-do compile }
! { dg-options "-std=legacy" }
      COMMON FOO(8)
      DO 10 M=1,8
        DO 10 T=0D0,1D0
          FOO(M)=1
   10 CONTINUE
      END
