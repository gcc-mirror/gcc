! { dg-do compile }
! { dg-additional-options "-Wdo-subscript" }
program main
  real, dimension(3) :: a
  a = 42.
  do i=-1,3 ! { dg-warning "out of bounds \\(-1 < 1\\)" }
     select case(i)
     case(1:3)
        a(i) = -234  ! { dg-warning "out of bounds \\(-1 < 1\\)" }
     end select
  end do
  do i=1,4,2
     a(i) = a(i)*2 ! No warning - end value is 3
  end do
  do i=1,4  ! { dg-warning "out of bounds \\(4 > 3\\)" }
     if (i > 3) exit
     a(i) = 33  ! { dg-warning "out of bounds \\(4 > 3\\)" }
  end do
  do i=0,3  ! { dg-warning "out of bounds \\(0 < 1\\)" }
    if (i < 1) cycle
    a(i) = -21. ! { dg-warning "out of bounds \\(0 < 1\\)" }
  end do
end program main
