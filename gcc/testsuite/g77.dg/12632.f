C { dg-do compile }
C { dg-options "-fbounds-check" }
       INTEGER I(1)
       I(2) = 0  ! { dg-error "out of defined range" "out of defined range" }
       END

