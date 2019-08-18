! { dg-do run }
program foo
   integer m, n, k
   m = merge_bits(b'010101', 1234, 42);   if (m /=  1232) stop 1
   n = merge_bits(1234, z'3456', 42);     if (n /= 13398) stop 2
   k = merge_bits(1234, 3456, o'12334');  if (k /=  3536) stop 3
end program foo
