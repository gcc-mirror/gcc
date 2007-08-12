! { dg-do run }
! { dg-shouldfail "" }
!
program trs
  implicit none
  integer :: size
  integer :: seed(50)
  call test_random_seed(size,seed)
contains
  subroutine test_random_seed(size, put, get)
    integer, optional :: size
    integer, dimension(:), optional :: put
    integer, dimension(:), optional :: get
    call random_seed(size, put, get)
  end subroutine test_random_seed
end program trs
! { dg-output "Fortran runtime error: RANDOM_SEED should have at most one argument present.*" }
