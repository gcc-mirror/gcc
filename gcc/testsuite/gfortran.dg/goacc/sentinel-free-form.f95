! { dg-do compile } 

program test
  implicit none

  integer :: i
  real :: x

  ! sentinel may only be preceeded by white space
  x = 0.0 !$acc parallel ! comment
  ! sentinel must appear as a single word
  ! $acc parallel ! comment
  !$ acc parallel ! { dg-error "Unclassifiable statement" }
  ! directive lines must have space after sentinel
  !$accparallel ! { dg-warning "followed by a space" }
  do i = 1,10
    x = x + 0.3
  enddo
  !$acc end parallel ! { dg-error "Unexpected" }
  print *, x
end