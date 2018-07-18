! { dg-do run }
!
program trs
  implicit none
  integer :: size, ierr
  integer, allocatable, dimension(:) :: seed, check
  call test_random_seed(size)
  allocate(seed(size),check(size))
  seed = 42
  call test_random_seed(put=seed)
  call test_random_seed(get=check)
  ! With xorshift1024* the last seed value is special
  seed(size) = check(size)
  if (any (seed /= check)) STOP 1
contains
  subroutine test_random_seed(size, put, get)
    integer, optional :: size
    integer, dimension(:), optional :: put
    integer, dimension(:), optional :: get
    call random_seed(size, put, get)
  end subroutine test_random_seed
end program trs
