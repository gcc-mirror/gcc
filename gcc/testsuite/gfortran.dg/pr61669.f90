! { dg-do compile }
      write (*,"(a)") char(12)
      CHARACTER*80 A /"A"/      ! { dg-error "Unexpected data declaration statement" }
      REAL*4 B                  ! { dg-error "Unexpected data declaration statement" }
      write (*,"(a)") char(12)
      DATA B / 0.02 /           ! { dg-warning "Obsolescent feature: DATA statement" }
      END
