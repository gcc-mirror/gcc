! { dg-do run }
!
! Test the behavior of lbound, ubound of shape with assumed rank arguments
! in an array context (without DIM argument).
!

program test

  integer              :: a(2:4,-2:5)
  integer, allocatable :: b(:,:)
  integer, pointer     :: c(:,:)
  character(52)        :: buffer

  call foo(a)

  allocate(b(2:4,-2:5))
  call foo(b)
  call bar(b)

  allocate(c(2:4,-2:5))
  call foo(c)
  call baz(c)

contains
  subroutine foo(arg)
    integer :: arg(..)

    !print *, lbound(arg)
    !print *, id(lbound(arg))
    if (any(lbound(arg) /= [1, 1])) STOP 1
    if (any(id(lbound(arg)) /= [1, 1])) STOP 2
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           1           1') STOP 3
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           1           1') STOP 4

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [3, 8])) STOP 5
    if (any(id(ubound(arg)) /= [3, 8])) STOP 6
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           3           8') STOP 7
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           3           8') STOP 8

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) STOP 9
    if (any(id(shape(arg)) /= [3, 8])) STOP 10
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') STOP 11
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') STOP 12

  end subroutine foo
  subroutine bar(arg)
    integer, allocatable :: arg(:,:)

    !print *, lbound(arg)
    !print *, id(lbound(arg))
    if (any(lbound(arg) /= [2, -2])) STOP 13
    if (any(id(lbound(arg)) /= [2, -2])) STOP 14
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           2          -2') STOP 15
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           2          -2') STOP 16

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [4, 5])) STOP 17
    if (any(id(ubound(arg)) /= [4, 5])) STOP 18
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           4           5') STOP 19
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           4           5') STOP 20

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) STOP 21
    if (any(id(shape(arg)) /= [3, 8])) STOP 22
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') STOP 23
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') STOP 24

  end subroutine bar
  subroutine baz(arg)
    integer, pointer :: arg(..)

    !print *, lbound(arg)
    !print *, id(lbound(arg))
    if (any(lbound(arg) /= [2, -2])) STOP 25
    if (any(id(lbound(arg)) /= [2, -2])) STOP 26
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           2          -2') STOP 27
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           2          -2') STOP 28

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [4, 5])) STOP 29
    if (any(id(ubound(arg)) /= [4, 5])) STOP 30
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           4           5') STOP 31
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           4           5') STOP 32

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) STOP 33
    if (any(id(shape(arg)) /= [3, 8])) STOP 34
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') STOP 35
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') STOP 36

  end subroutine baz
  elemental function id(arg)
    integer, intent(in) :: arg
    integer             :: id

    id = arg
  end function id
end program test

