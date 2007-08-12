! { dg-do compile }
!
subroutine test1 (size, put, get)
  integer :: size
  integer, dimension(:), optional :: put
  integer, dimension(:), optional :: get
  call random_seed(size, put, get)
end

subroutine test2 (size, put, get)
  integer, optional :: size
  integer, dimension(:) :: put
  integer, dimension(:) :: get
  call random_seed(size, put, get) ! { dg-error "Too many arguments" }
end
