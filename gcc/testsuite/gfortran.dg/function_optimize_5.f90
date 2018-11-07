! { dg-do compile }
! { dg-options "-ffrontend-optimize -faggressive-function-elimination -finline-matmul-limit=0 -Wfunction-elimination" }
! Check the -ffrontend-optimize (in the absence of -O) and
! -Wfunction-elimination options.
program main
  implicit none
  real, dimension(2,2) :: a, b, c, d
  integer :: i
  real :: x, z
  character(60) :: line
  real, external :: ext_func
  interface
     elemental function element(x)
       real, intent(in) :: x
       real :: elem
     end function element
     pure function mypure(x)
       real, intent(in) :: x
       integer :: mypure
     end function mypure
     elemental impure function elem_impure(x)
       real, intent(in) :: x
       real :: elem_impure
     end function elem_impure
  end interface

  data a /2., 3., 5., 7./
  data b /11., 13., 17., 23./
  write (unit=line, fmt='(4F7.2)') matmul(a,b)  &
       & + matmul(a,b)
  z = sin(x) + 2.0 + sin(x)
  print *,z
  x = ext_func(a) + 23 + ext_func(a)  ! { dg-warning "Removing call to impure function" }
  print *,d,x
  z = element(x) + element(x)
  print *,z
  i = mypure(x) - mypure(x)
  print *,i
  z = elem_impure(x) - elem_impure(x)  ! { dg-warning "Removing call to impure function" }
  print *,z
end program main
