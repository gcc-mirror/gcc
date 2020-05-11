! { dg-do compile }
! PR fortran/95053 - ICE in gfc_divide(): Bad basic type
!
 123  FORMAT ('A'/'B')
 132  FORMAT (A/
     +     ' B')
      END
