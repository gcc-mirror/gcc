! { dg-do compile }
! { dg-options "-funsigned" }
program memain
  unsigned :: a
  a = 1u
  print *,complex(1.0,a) !  { dg-error "shall not be UNSIGNED" }
  print *,complex(a,1.0) !  { dg-error "shall not be UNSIGNED" }
end program memain
