! { dg-do compile }
!
! PR fortran/45066
!
! Contributed by Michael Richmond.
!
! Was failing due to a -fwhole-file bug.
!

MODULE GA_commons
  INTEGER :: nichflg(2)
END MODULE GA_commons

PROGRAM gafortran
  USE GA_commons
  NAMELIST /ga/ nichflg
  READ (23, nml=ga)
END PROGRAM gafortran
