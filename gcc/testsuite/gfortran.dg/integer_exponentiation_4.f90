! { dg-do compile }
! { dg-options "" }
program test
  implicit none

!!!!!! INTEGER BASE !!!!!!
  print *, 0**0
  print *, 0**1
  print *, 0**(-1) ! { dg-error "Division by zero" }
  print *, 0**(huge(0))
  print *, 0**(-huge(0)-1) ! { dg-error "Division by zero" }
  print *, 0**(2_8**32)
  print *, 0**(-(2_8**32)) ! { dg-error "Division by zero" }

  print *, 1**huge(0)
  print *, 1**(-huge(0)-1)
  print *, 1**huge(0_8)
  print *, 1**(-huge(0_8)-1_8)
  print *, (-1)**huge(0)
  print *, (-1)**(-huge(0)-1)
  print *, (-1)**huge(0_8)
  print *, (-1)**(-huge(0_8)-1_8)

  print *, 2**huge(0) ! { dg-error "Arithmetic overflow|exceeds the range" }
  print *, 2**huge(0_8) ! { dg-error "Arithmetic overflow|exceeds the range" }
  print *, (-2)**huge(0) ! { dg-error "Arithmetic overflow|exceeds the range" }
  print *, (-2)**huge(0_8) ! { dg-error "Arithmetic overflow|exceeds the range" }

  print *, 2**(-huge(0)-1)
  print *, 2**(-huge(0_8)-1_8)
  print *, (-2)**(-huge(0)-1)
  print *, (-2)**(-huge(0_8)-1_8)

!!!!!! REAL BASE !!!!!!
  print *, 0.0**(-1) ! { dg-error "Arithmetic overflow" }
  print *, 0.0**(-huge(0)-1) ! { dg-error "Arithmetic overflow" }
  print *, 2.0**huge(0) ! { dg-error "Arithmetic overflow" }
  print *, nearest(1.0,-1.0)**(-huge(0)) ! { dg-error "Arithmetic overflow" }

!!!!!! COMPLEX BASE !!!!!!
  print *, (2.0,-4.3)**huge(0) ! { dg-error "Arithmetic overflow" }
  print *, (2.0,-4.3)**huge(0_8) ! { dg-error "Arithmetic overflow" }
  print *, (2.0,-4.3)**(-huge(0))
  print *, (2.0,-4.3)**(-huge(0_8))

end program test
