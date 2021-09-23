! { dg-do compile }
! { dg-additional-options "-march=armv8.3-a" { target aarch64-*-* } }

subroutine cppco(ap, z)
  implicit none
  complex :: ap(*), z(*)

  z(1) = z(1) + z(2) * (ap(1))

  return
end subroutine cppco
