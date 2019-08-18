! { dg-do run }
program foo
   integer :: k = 42
   n = iand(k, z'3456'); if (n /=  2) stop 1
   n = iand(z'1234', k); if (n /= 32) stop 2
   n =  and(k, z'3456'); if (n /=  2)  stop 3
   n =  and(z'1234', k); if (n /=  32) stop 4
   n = ieor(k, z'3456'); if (n /= 13436) stop 5
   n = ieor(z'1234', k); if (n /=  4638) stop 6
   n =  xor(k, z'3456'); if (n /= 13436) stop 7
   n =  xor(z'1234', k); if (n /=  4638) stop 8
   n =  ior(k, z'3456'); if (n /= 13438) stop 9
   n =  ior(z'1234', k); if (n /=  4670) stop 10
   n =   or(k, z'3456'); if (n /= 13438) stop 11
   n =   or(z'1234', k); if (n /=  4670) stop 12
end program foo

