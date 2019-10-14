! { dg-do compile }
! PR fortran/92019
program foo
  integer :: a(4) = [1, 2, 3, 4]
  print *, a(z'1')            ! { dg-error "Invalid BOZ literal constant" }
  print *, a(1:z'3')          ! { dg-error "Invalid BOZ literal constant" }
  print *, a(1:2:z'2')        ! { dg-error "Invalid BOZ literal constant" }
  print *, a([z'2',z'1'])     ! { dg-error "cannot appear in an array constructor" }
end program foo
