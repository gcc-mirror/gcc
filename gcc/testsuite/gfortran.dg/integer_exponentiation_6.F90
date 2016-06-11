! { dg-options "-fno-range-check" }
program test
  write (*,*) (2_8 ** 64009999_8) / 2
end program test
