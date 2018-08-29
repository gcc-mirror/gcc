! { dg-do run }
!
! Testcase for PR fortran/65841
! Contributed by Damian Rousson
!
program alloc_comp_deep_copy_2
  type a
    real, allocatable :: f
  end type
  type b
    type(a), allocatable :: g
  end type

  type(b) c,d

  c%g=a(1.) 
  d=c
  if (d%g%f /= 1.0) STOP 1
  d%g%f = 2.0
  if (d%g%f /= 2.0) STOP 2
end program
