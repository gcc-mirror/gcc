! { dg-do compile }
  double precision, parameter :: x = huge(1d0)
  print*, sum((/x,-x/))
  print*, sum((/x,x,-x,-x/))  ! { dg-error "overflow" }
  print*, sum((/x,-x,1d0/))
  print*, sum((/1d0,x,-x/))
end
