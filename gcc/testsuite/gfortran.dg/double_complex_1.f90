! { dg-do compile }
! { dg-options "--std=f95" }
! PR18565
! As we provide "double complex" versions of certain intrinsics an extension.
! However --std=f95 was also breaking the generic versions, which should work
! on any type kind.
program prog
  complex(kind=kind(0d0)) :: c
  print *, abs(c)
  print *, aimag(c)
  print *, conjg(c)
  print *, cos(c)
  print *, exp(c)
  print *, log(c)
  print *, sin(c)
  print *, sqrt(c)
end program

