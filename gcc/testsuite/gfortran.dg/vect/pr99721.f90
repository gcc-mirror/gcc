! { dg-do compile }
! { dg-additional-options "-O3" }
! { dg-additional-options "-march=armv8.3-a" { target aarch64-*-* } }
subroutine sub_c
  complex, dimension(2,3) :: at
  complex, dimension(2,4) :: b
  complex, dimension(3,4) :: c
  data b / (41., 43.), 0, 0, 0, 0, 0, 0, 0/
  c = matmul(transpose(at), b)
  if (any (c /= cres)) stop
end subroutine sub_c
