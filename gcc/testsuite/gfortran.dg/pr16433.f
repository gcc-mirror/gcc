! { dg-do compile }
      real x
      double precision dx
      data x/x'2ffde'/ ! { dg-error "Hexadecimal constant" } 
      dx = x
      end
