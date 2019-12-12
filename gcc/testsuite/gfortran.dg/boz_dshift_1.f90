! { dg-do compile }
program foo
   integer k, n
   k = dshiftl(z'1234',z'2345',1)   ! { dg-error "cannot both be BOZ" }
   n = dshiftr(z'1234',z'2345',1)   ! { dg-error "cannot both be BOZ" }
   if (k .eq. n) stop 1
   k = dshiftl(z'1234',3.1415,1)   ! { dg-error "must be INTEGER" }
   n = dshiftr(2.7362,z'2345',1)   ! { dg-error "must be INTEGER" }
   if (k .eq. n) stop 2
end program foo
