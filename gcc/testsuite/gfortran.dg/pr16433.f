! { dg-do compile }
      real x
      double precision dx
      data x/x'2ffde'/ ! { dg-warning "Hexadecimal constant | used to initialize non-integer" } 
      dx = x  ! { dg-bogus "exadecimal constant" "Hex constant where there is none" }
      end
