! PR fortran/78758
! { dg-do compile }
! { dg-options "-O2 -Wall" }

integer function pr78758 (x)
  character(len=*), intent(in) :: x
  character(len=16)            :: y
  integer, external            :: z
  y(2:) = " " // adjustl (x(2:))
  pr78758 = z (y)
end function pr78758
