program strarray_4
  character(len=5), dimension(2) :: c
  
  c(1) = "Hello"
  c(2) = "World"

  call foo1(c)
  call foo2(c, 2)
  call foo3(c, 5, 2)
contains
subroutine foo1(a)
  implicit none
  character(len=5), dimension(2)  :: a
  character(len=5), dimension(2)  :: b

  b = a;
  if ((b(1) .ne. "Hello") .or. (b(2) .ne. "World")) call abort
end subroutine

subroutine foo2(a, m)
  implicit none
  integer  m
  character(len=5), dimension(m)  :: a
  character(len=5), dimension(m)  :: b

  b = a
  if ((b(1) .ne. "Hello") .or. (b(2) .ne. "World")) call abort
end subroutine

subroutine foo3(a, n, m)
  implicit none
  integer n, m
  character(len=n), dimension(m)  :: a
  character(len=n), dimension(m)  :: b

  b = a
  if ((b(1) .ne. "Hello") .or. (b(2) .ne. "World")) call abort
end subroutine
end program
