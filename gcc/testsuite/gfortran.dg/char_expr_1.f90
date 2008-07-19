! { dg-do "run" }
! PR fortran/36795
! "(str)" (= an expression) was regarded as "str" (= a variable)
! and thus when yy was deallocated so was xx. Result: An invalid
! memory access.
!
program main
  implicit none
  character (len=10), allocatable :: str(:)
  allocate (str(1))
  str(1)      = "dog"
  if (size(str) /= 1 .or. str(1) /= "dog") call abort()
contains
  subroutine foo(xx,yy)
    character (len=*), intent(in)               :: xx(:)
    character (len=*), intent(out), allocatable :: yy(:)
    allocate (yy(size(xx)))
    yy = xx
  end subroutine foo
end program main
