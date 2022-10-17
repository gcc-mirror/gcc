! PR c/101297

module m
implicit none
integer :: i
contains
subroutine foo ()
  !$omp atomic update,	! { dg-error "Clause expected at .1. after trailing comma" }
  i = i + 1
  !$omp atomic update,,	! { dg-error "Failed to match clause" }
  i = i + 1
end
end module
