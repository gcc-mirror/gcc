program strarray_3
  character(len=5), dimension(2) :: c
  
  c(1) = "Hello"
  c(2) = "World"

  call foo1(c)
  call foo2(c, 2)
  call foo3(c, 5)
  call foo4(c, 5, 2)
  call foo5(c(2:1:-1))
contains
subroutine foo1(a)
  implicit none
  character(len=5), dimension(2)  :: a

  if ((a(1) .ne. "Hello") .or. (a(2) .ne. "World")) STOP 1
end subroutine

subroutine foo2(a, m)
  implicit none
  integer  m
  character(len=5), dimension(m)  :: a

  if ((a(1) .ne. "Hello") .or. (a(2) .ne. "World")) STOP 2
end subroutine

subroutine foo3(a, n)
  implicit none
  integer n
  character(len=n), dimension(:)  :: a

  if ((a(1) .ne. "Hello") .or. (a(2) .ne. "World")) STOP 3
end subroutine

subroutine foo4(a, n, m)
  implicit none
  integer n, m
  character(len=n), dimension(m)  :: a

  if ((a(1) .ne. "Hello") .or. (a(2) .ne. "World")) STOP 4
end subroutine

subroutine foo5(a)
  implicit none
  character(len=2), dimension(5)  :: a

  if ((a(1) .ne. "Wo") .or. (a(3) .ne. "dH") .or. (a(5) .ne. "lo")) STOP 5
end subroutine
end program
