! { dg-do compile }
! { dg-additional-options "-march=armv8.3-a -O3" { target { aarch64*-*-* } } }

program main
  complex, dimension(3, 2) :: a
  complex, dimension(2, 4) :: b
  complex, dimension(3, 4) :: c, res1

  data a /0, (-5., -7.), (11., -13.), 0, 0, 0/
  data b /0, 0, 0, 0, 0, 0, 0, 0/
  data res1 /0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/

  c = matmul(a, conjg(b))
  if (any(res1 /= c)) stop 2
end program main
