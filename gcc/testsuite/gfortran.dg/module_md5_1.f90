! Check that we can write a module file, that it has a correct MD5 sum,
! and that we can read it back.
!
! { dg-do compile }
module foo
  integer(kind=4), parameter :: pi = 3_4
end module foo

program test
  use foo
  print *, pi
end program test
! { dg-final { scan-module "foo" "MD5:9c43cf4d713824ec6894b83250720e68" } }
! { dg-final { cleanup-modules "foo" } }
