! { dg-do compile }
!
! PR fortran/60543
! PR fortran/60283
!
module m
contains
  REAL(8) FUNCTION random()
    CALL RANDOM_NUMBER(random)
  END FUNCTION random
  REAL(8) FUNCTION random2()
    block
      block
        block
          CALL RANDOM_NUMBER(random2)
        end block
      end block
    end block
  END FUNCTION random2
end module m

! { dg-final { scan-module-absence "m" "IMPLICIT_PURE" } }
