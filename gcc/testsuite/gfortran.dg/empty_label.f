C { dg-do compile }
C { dg-options "-Werror -fmax-errors=1" }
100   ! { dg-warning "empty statement" }
      end
C { dg-error "count reached limit" "" { target *-*-* } 0 }
