! { dg-do compile }
program foo
   integer m
   m = merge_bits(b'010101', b"101010", 42) ! { dg-error "cannot both be" }
end program foo
