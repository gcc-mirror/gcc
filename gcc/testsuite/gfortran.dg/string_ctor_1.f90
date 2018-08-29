! { dg-do run }
! { dg-options "-std=legacy" }
!
! Program to test character array constructors.
! PR17144
subroutine test1 (n, t, u)
  integer n
  character(len=n) :: s(2)
  character(len=*) :: t
  character(len=*) :: u

  ! A variable array constructor.
  s = (/t, u/)
  ! An array constructor as part of an expression.
  if (any (s .ne. (/"Hell", "Worl"/))) STOP 1
end subroutine

subroutine test2
  character*5 :: s(2)

  ! A constant array constructor
  s = (/"Hello", "World"/)
  if ((s(1) .ne. "Hello") .or. (s(2) .ne. "World")) STOP 2
end subroutine

subroutine test3
  character*1 s(26)
  character*26 t
  integer i

  ! A large array constructor
  s = (/'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', &
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'/)
  do i=1, 26
    t(i:i) = s(i)
  end do

  ! Assignment with dependency
  s = (/(s(27-i), i=1, 26)/)
  do i=1, 26
    t(i:i) = s(i)
  end do
  if (t .ne. "zyxwvutsrqponmlkjihgfedcba") STOP 3
end subroutine

program string_ctor_1
  call test1 (4, "Hello", "World")
  call test2
  call test3
end program

