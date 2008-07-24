! { dg-do compile }
! { dg-options "-Warray-temporaries" }

subroutine bar(a)
  real, dimension(2) :: a
end

program main
  integer, parameter :: n=3
  integer :: i
  real, dimension(n) :: a, b

  a = 0.2
  i = 2
  a(i:i+1) = a(1:2) ! { dg-warning "Creating array temporary" }
  a = cshift(a,1) ! { dg-warning "Creating array temporary" }
  b = cshift(a,1) 
  call bar(a(1:3:2)) ! { dg-warning "Creating array temporary" }
end program main
