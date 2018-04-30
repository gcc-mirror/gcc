! { dg-do run }
! PR61173.f90 Bogus END condition
module bd
  character(len=25, kind=1), dimension(:), allocatable, save :: source
  contains
    subroutine init_data
      allocate(source(2))
      source=["   1   1   1  ", "   4   4   4  "]
    end subroutine init_data
end module bd
program read_internal
  use bd
  integer :: x(6),i

  call init_data
  read(source,*) (x(i), i=1,6)
  if (any(x/=[1,1,1,4,4,4])) STOP 1
end program read_internal
