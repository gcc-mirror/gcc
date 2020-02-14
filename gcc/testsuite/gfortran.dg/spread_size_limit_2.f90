! PR fortran/91944
! { dg-do compile }
! { dg-options "-fmax-array-constructor=65535" }

program pr91944
  integer, parameter :: n = 10
  integer, parameter :: m = 65536
  integer :: i
  integer :: x(n,m) = spread([(i,i=1,n)], dim=2, ncopies=m)	! { dg-error "requires an increase of the allowed 65535 upper limit" }
  print *, x(n,m)
end
