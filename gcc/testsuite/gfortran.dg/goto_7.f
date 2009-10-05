! { dg-do compile }
! { dg-options "-std=legacy" }

! Check for error message when computed and assigned gotos reference
! illegal label numbers.

      ASSIGN 1 TO I
      GOTO (1, 2, 3, 42), 2 ! { dg-error "is never defined" }
      GOTO I, (1, 2, 3, 43) ! { dg-error "is never defined" }
 1    CONTINUE
 2    CONTINUE
 3    CONTINUE
c     No label 42 or 43.
      END
