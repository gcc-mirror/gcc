program A_M
  implicit none
  real, parameter :: C(5:10) = 5.0
  real, dimension (:), allocatable :: A, B
  allocate (A(6))
  call Init (A)
contains
  subroutine Init ( A )
    real, dimension ( -1 : ), intent ( in ) :: A
    integer, dimension ( 1 ) :: lb_B

    allocate (B, mold = A)
    if (any (lbound (B) /= lbound (A))) stop 1
    if (any (ubound (B) /= ubound (A))) stop 2
    if (any (shape (B) /= shape (A))) stop 3
    if (size (B) /= size (A)) stop 4
    deallocate (B)
    allocate (B, mold = C)
    if (any (lbound (B) /= lbound (C))) stop 5
    if (any (ubound (B) /= ubound (C))) stop 6
    if (any (shape (B) /= shape (C))) stop 7
    if (size (B) /= size (C)) stop 8
end
end 
