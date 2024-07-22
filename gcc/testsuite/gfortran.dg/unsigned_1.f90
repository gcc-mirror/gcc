! { dg-do compile }
! { dg-options "-funsigned" }
! A first, very simple program, that should compile.
program memain
  unsigned :: u
  u = 1U
  u = 2u
end program memain
