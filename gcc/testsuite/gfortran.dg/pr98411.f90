! { dg-do compile }
! { dg-options "-std=f2008 -Wall -fautomatic -fmax-stack-var-size=100" }
! PR fortran/98411 - Pointless warning for static variables 

module try
  implicit none
  integer, save :: a(1000)
contains
  subroutine initmodule
    real, save :: b(1000)
    logical    :: c(1000) ! { dg-warning "moved from stack to static storage" }
    integer    :: e(1000) = 1
    a(1) = 42
    b(2) = 3.14
    c(3) = .true.
    e(5) = -1
  end subroutine initmodule
end module try
