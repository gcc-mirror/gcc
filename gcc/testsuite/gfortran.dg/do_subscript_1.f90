! { dg-do compile }
program main
  real, dimension(3) :: a
  a = 42.
  do i=-1,3,2 ! { dg-warning "out of bounds" }
     a(i) = 0  ! { dg-warning "out of bounds \\(-1 < 1\\)" }
  end do
  do i=4,1,-1 ! { dg-warning "out of bounds" }
     a(i) = 22 ! { dg-warning "out of bounds \\(4 > 3\\)" }
  end do
  do i=1,4 ! { dg-warning "out of bounds" }
     a(i) = 32 ! { dg-warning "out of bounds \\(4 > 3\\)" }
  end do
  do i=3,0,-1 ! { dg-warning "out of bounds" }
     a(i) = 12 ! { dg-warning "out of bounds \\(0 < 1\\)" }
  end do
  do i=-1,3
     if (i>0) a(i) = a(i) + 1 ! No warning inside if
  end do
  do i=-1,4
     select case(i)
     case(1:3)
        a(i) = -234  ! No warning inside select case
     end select
  end do
  do i=1,3 ! { dg-warning "out of bounds" }
     a(i+1) = a(i) ! { dg-warning "out of bounds \\(4 > 3\\)" }
     a(i-1) = a(i) ! { dg-warning "out of bounds \\(0 < 1\\)" }
  end do
  do i=3,1,-1 ! { dg-warning "out of bounds" }
     a(i) = a(i-1) ! { dg-warning "out of bounds \\(0 < 1\\)" }
     a(i) = a(i+1) ! { dg-warning "out of bounds \\(4 > 3\\)" }
  end do
  do i=1,2 ! { dg-warning "out of bounds" }
     a(i) = a(i*i) ! { dg-warning "out of bounds \\(4 > 3\\)" }
  end do
  do i=1,4,2
     a(i) = a(i)*2 ! No error
  end do
  do i=1,4
     if (i > 3) exit
     a(i) = 33
  end do
  do i=0,3 ! { dg-warning "out of bounds \\(0 < 1\\)" }
    a(i) = 13.  ! { dg-warning "out of bounds \\(0 < 1\\)" }
    if (i < 1) exit
  end do
  do i=0,3
    if (i < 1) cycle
    a(i) = -21.
  end do
  do i=0,3 ! { dg-warning "out of bounds \\(0 < 1\\)" }
    do j=1,2
       a(i) = -123 ! { dg-warning "out of bounds \\(0 < 1\\)" }
    end do
  end do
end program main
