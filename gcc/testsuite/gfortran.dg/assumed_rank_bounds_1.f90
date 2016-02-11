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
    if (any(lbound(arg) /= [1, 1])) call abort
    if (any(id(lbound(arg)) /= [1, 1])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           1           1') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           1           1') call abort

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [3, 8])) call abort
    if (any(id(ubound(arg)) /= [3, 8])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           3           8') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           3           8') call abort

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) call abort
    if (any(id(shape(arg)) /= [3, 8])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') call abort

  end subroutine foo
  subroutine bar(arg)
    integer, allocatable :: arg(:,:)

    !print *, lbound(arg)
    !print *, id(lbound(arg))
    if (any(lbound(arg) /= [2, -2])) call abort
    if (any(id(lbound(arg)) /= [2, -2])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           2          -2') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           2          -2') call abort

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [4, 5])) call abort
    if (any(id(ubound(arg)) /= [4, 5])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           4           5') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           4           5') call abort

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) call abort
    if (any(id(shape(arg)) /= [3, 8])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') call abort

  end subroutine bar
  subroutine baz(arg)
    integer, pointer :: arg(..)

    !print *, lbound(arg)
    !print *, id(lbound(arg))
    if (any(lbound(arg) /= [2, -2])) call abort
    if (any(id(lbound(arg)) /= [2, -2])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) lbound(arg)
    if (buffer /= '           2          -2') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(lbound(arg))
    if (buffer /= '           2          -2') call abort

    !print *, ubound(arg)
    !print *, id(ubound(arg))
    if (any(ubound(arg) /= [4, 5])) call abort
    if (any(id(ubound(arg)) /= [4, 5])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) ubound(arg)
    if (buffer /= '           4           5') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(ubound(arg))
    if (buffer /= '           4           5') call abort

    !print *, shape(arg)
    !print *, id(shape(arg))
    if (any(shape(arg) /= [3, 8])) call abort
    if (any(id(shape(arg)) /= [3, 8])) call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) shape(arg)
    if (buffer /= '           3           8') call abort
    buffer = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz'
    write(buffer,*) id(shape(arg))
    if (buffer /= '           3           8') call abort

  end subroutine baz
  elemental function id(arg)
    integer, intent(in) :: arg
    integer             :: id

    id = arg
  end function id
end program test

