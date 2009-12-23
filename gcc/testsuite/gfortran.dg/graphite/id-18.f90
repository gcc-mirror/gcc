MODULE spherical_harmonics
  INTEGER, PARAMETER :: dp = SELECTED_REAL_KIND ( 14, 200 )
CONTAINS
  FUNCTION dlegendre (x, l, m) RESULT (dplm)
  SELECT CASE ( l )
  CASE ( 0 )
    dplm = 0.0_dp
  CASE ( 1 )
    dplm = 1.0_dp
  CASE DEFAULT
    IF ( mm > 0 ) THEN
      dpmm = -m
      DO im = 1, mm
        dpmm = -dpmm
      END DO
      IF ( l == mm + 1 ) THEN
        DO il = mm + 2, l
          dpll = dpmm
        END DO
        dplm = dpll
      END IF
    END IF
  END SELECT
  END FUNCTION dlegendre
END MODULE spherical_harmonics
