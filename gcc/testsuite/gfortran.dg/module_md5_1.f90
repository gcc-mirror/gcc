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
! { dg-final { scan-module "foo" "MD5:596df8f39d3ddc0b847771cadcb26274" } }
! { dg-final { cleanup-modules "foo" } }
