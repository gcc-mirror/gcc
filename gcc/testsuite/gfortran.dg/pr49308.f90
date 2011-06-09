! PR middle-end/49308
! { dg-do compile }
! { dg-options "-O2 -funroll-loops -g" }

subroutine foo(n, b, d, e)
  type t
    integer :: f
  end type t
  type s
    type(t), pointer :: g
  end type s
  type u
    type(s), dimension(:), pointer :: h
  end type
  integer :: i, k, n
  type(u), pointer :: a, e
  character(len=250) :: b, c, d
  logical :: l
  do i = 1, n
    j = i - 1
    if (j/=0) c = trim(b) // adjustl(d(j))
  end do
  a => e
  do k = 1, size(a%h)
     l = (a%h(k)%g%f == a%h(1)%g%f)
     if (.not.(l)) call bar()
  enddo
end subroutine foo
