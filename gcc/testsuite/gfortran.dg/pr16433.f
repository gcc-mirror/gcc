! { dg-do-compile }
      real x
      double precision dx
      x = x'2ffde' ! { dg-warning "exadecimal constant" "Hex constant can't begin with x" }
      dx = x  ! { dg-bogus "exadecimal constant" "Hex constant where there is none" }
      end
