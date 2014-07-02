C { dg-do compile }
      TYPE T
      INTEGER A(2)/1,2/ ! { dg-error "Invalid old style initialization for derived type component" }
      END TYPE
      TYPE S
      INTEGER B/1/ ! { dg-error "Invalid old style initialization for derived type component" }
      END TYPE
      END
