! { dg-do run }

! PR fortran/96992

! Contributed by Thomas Koenig  <tkoenig@gcc.gnu.org>

! From the standard:
! An actual argument that represents an element sequence and
! corresponds to a dummy argument that is an array is sequence
! associated with the dummy argument. The rank and shape of the
! actual argument need not agree with the rank and shape of the
! dummy argument, but the number of elements in the dummy argument
! shall not exceed the number of elements in the element sequence
! of the actual argument. If the dummy argument is assumed-size,
! the number of elements in the dummy argument is exactly
! the number of elements in the element sequence.

! Check that walking the sequence starts with an initialized stride
! for dim == 0.

module foo_mod

  implicit none

  type foo
     integer :: i
  end type foo

contains

  subroutine d1(x,n)
    integer, intent(in) :: n
    integer :: i
    class (foo), intent(out), dimension(n) :: x

    x(:)%i = (/ (42 + i, i = 1, n ) /)
  end subroutine d1

  subroutine d2(x,n,sb)
    integer, intent(in) :: n
    integer :: i, sb
    class (foo), intent(in), dimension(n,n,n) :: x

    if ( any( x%i /= reshape((/ (42 + i, i = 1, n ** 3 ) /), [n, n, n] ))) stop sb + 1
  end subroutine d2

  subroutine d3(x,n)
    integer, intent(in) :: n
    integer :: i
    class (foo), intent(inout) :: x(n)

    x%i = -x%i               ! Simply negate elements
  end subroutine d3

  subroutine d4(a,n)
    integer, intent(in) :: n
    class (foo), intent(inout) :: a(*)

    call d3(a,n)
  end subroutine d4
  
  subroutine d1s(x,n, sb)
    integer, intent(in) :: n, sb
    integer :: i
    class (*), intent(out), dimension(n) :: x

    select type(x)
    class is(foo)
       x(:)%i = (/ (42 + i, i = 1, n ) /)
    class default
       stop sb + 2
    end select
  end subroutine d1s

  subroutine d2s(x,n,sb)
    integer, intent(in) :: n,sb
    integer :: i
    class (*), intent(in), dimension(n,n,n) :: x

    select type (x)
    class is (foo)
       if ( any( x%i /= reshape((/ (42 + i, i = 1, n ** 3 ) /), [n, n, n] ))) stop sb + 3
    class default
       stop sb + 4
    end select
  end subroutine d2s

  subroutine d3s(x,n,sb)
    integer, intent(in) :: n, sb
    integer :: i
    class (*), intent(inout) :: x(n)

    select type (x)
    class is (foo)
       x%i = -x%i               ! Simply negate elements
    class default
       stop sb + 5
    end select
  end subroutine d3s

end module foo_mod

program main

  use foo_mod

  implicit none

  type (foo), dimension(:), allocatable :: f
  type (foo), dimension(27) :: g
  type (foo), dimension(3, 9) :: td
  integer :: n,i,np3

  n = 3
  np3 = n **3
  allocate (f(np3))
  call d1(f, np3)
  call d2(f, n, 0)

  call d1s(f, np3, 0)
  call d2s(f, n, 0)

  ! Use negative stride
  call d1(f(np3:1:-1), np3)
  if ( any( f%i /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 6
  call d2(f(np3:1:-1), n, 0)
  call d3(f(1:np3:4), np3/4)
  if ( any( f%i /= (/ (merge(-(42 + (np3 - i)),  & 
                               42 + (np3 - i),   &
                             MOD(i, 4) == 0 .AND. i < 21), &
                       i = 0, np3 - 1 ) /) ))    &
    stop 7
  call d4(f(1:np3:4), np3/4)
  if ( any( f%i /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 8

  call d1s(f(np3:1:-1), np3, 0)
  if ( any( f%i /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 9
  call d2s(f(np3:1:-1), n, 0)
  call d3s(f(1:np3:4), np3/4, 0)
  if ( any( f%i /= (/ (merge(-(42 + (np3 - i)),  & 
                               42 + (np3 - i),   &
                             MOD(i, 4) == 0 .AND. i < 21), &
                       i = 0, np3 - 1 ) /) ))    &
    stop 10

  deallocate (f)

  call d1(g, np3)
  call d2(g, n, 11)

  call d1s(g, np3, 11)
  call d2s(g, n, 11)

  ! Use negative stride
  call d1(g(np3:1:-1), np3)
  if ( any( g%i /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 17
  call d2(g(np3:1:-1), n, 11)
  call d3(g(1:np3:4), np3/4)
  if ( any( g%i /= (/ (merge(-(42 + (np3 - i)),  & 
                               42 + (np3 - i),   &
                             MOD(i, 4) == 0 .AND. i < 21), &
                       i = 0, np3 - 1 ) /) ))    &
    stop 18

  call d1s(g(np3:1:-1), np3, 11)
  if ( any( g%i /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 19
  call d2s(g(np3:1:-1), n, 11)
  call d3s(g(1:np3:4), np3/4, 11)
  if ( any( g%i /= (/ (merge(-(42 + (np3 - i)),  & 
                               42 + (np3 - i),   &
                             MOD(i, 4) == 0 .AND. i < 21), &
                       i = 0, np3 - 1 ) /) ))    &
    stop 20

  ! Check for 2D
  call d1(td, np3)
  call d2(td, n, 21)

  call d1s(td, np3, 21)
  call d2s(td, n, 21)

  ! Use negative stride
  call d1(td(3:1:-1,9:1:-1), np3)
  if ( any( reshape(td%i, [np3]) /= (/ (42 + i, i = np3, 1, -1 ) /) )) stop 26
  call d2(td(3:1:-1,9:1:-1), n, 21)
  call d3(td(2,1:n), n)
  if ( any( reshape(td%i, [np3]) /= (/ (merge(-(42 + (np3 - i)),  & 
                               42 + (np3 - i),   &
                             MOD(i, 3) == 1 .AND. i < 9), &
                       i = 0, np3 - 1 ) /) ))    &
    stop 27

end program main

