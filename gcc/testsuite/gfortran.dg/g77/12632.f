C { dg-do compile }
C { dg-options "-fbounds-check" }
       INTEGER I(1)
       I(2) = 0  ! { dg-error "out of bounds" "out of bounds" }
       END

