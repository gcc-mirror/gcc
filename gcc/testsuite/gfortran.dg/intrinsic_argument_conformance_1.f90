! { dg-do compile }
program main
  real :: av(2), bv(4)
  real :: a(2,2)
  logical :: lo(3,2)
  print *,dot_product(av, bv) ! { dg-error "different shape" }
  print *,pack(a, lo) ! { dg-error "different shape" }
  print *,merge(av, bv, lo(1,:)) ! { dg-error "different shape" }
  print *,matmul(bv,a) ! { dg-error "different shape" }
end program main
