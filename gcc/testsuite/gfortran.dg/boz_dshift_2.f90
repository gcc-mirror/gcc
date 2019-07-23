! { dg-do run }
program foo
   integer k, n
   k = dshiftl(z'1234',42,1)
   n = dshiftr(z'1234',42,1)
   if (k /= 9320) stop 1
   if (n /= 21) stop 2
   k = dshiftl(42,b'01010101', 1)
   n = dshiftr(22,o'12345', 1)
   if (k /= 84) stop 1
   if (n /= 2674) stop 2
end program foo
