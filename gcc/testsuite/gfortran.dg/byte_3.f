c { dg-do run }
c { dg-options "-std=legacy" }
      bytea
      a = 1
      if (a /= 1 .and. kind(a) /= a) stop 1
      end
