! { dg-do compile }
program main
  real, dimension(4,3) :: a
  real, dimension(2) :: b
  a = 21.
  b = product(a,dim=1) ! { dg-error "Different shape" }
  b = sum(a,dim=2) ! { dg-error "Different shape" }
end program main
