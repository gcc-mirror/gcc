! { dg-do compile }
!
! PR 47023: [4.6/4.7 regression] C_Sizeof: Rejects valid code
!
! Contributed by <florian.rathgeber@gmail.com>

  use iso_c_binding
  real(c_double) x
  print *, c_sizeof(x)
  print *, c_sizeof(0.0_c_double)
end 
