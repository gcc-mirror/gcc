! { dg-do compile }
program p
   integer :: a(2), b(2), c(2)
   data a /2*b1'/ ! { dg-error "must be a PARAMETER in DATA" }
   data b /2*o1'  ! { dg-error "must be a PARAMETER in DATA" }
   data c /2*z1   ! { dg-error "must be a PARAMETER in DATA" }
end
