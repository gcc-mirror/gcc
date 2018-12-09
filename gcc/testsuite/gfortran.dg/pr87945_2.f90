! { dg-do compile }
! PR fortran/87945
program p
   character :: a, b
   a%len = 1      ! { dg-error "to a constant expression" }
   b%kind = 'b'   ! { dg-error "to a constant expression" }
end
