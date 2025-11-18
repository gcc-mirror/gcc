! { dg-do compile }
      write (*,"(a)") char(12)
      CHARACTER*80 A /"A"/      ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
      REAL*4 B                  ! { dg-error "data declaration statement at \\(1\\) cannot appear after executable statements" }
      write (*,"(a)") char(12)
      DATA B / 0.02 /           ! { dg-warning "Obsolescent feature: DATA statement" }
      END
