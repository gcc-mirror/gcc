! { dg-do run }
!
! Passing CLASS to TYPE
!
implicit none
type t
  integer :: A
  real, allocatable :: B(:)
end type t

type, extends(t) ::  t2
  complex :: z = cmplx(3.3, 4.4)
end type t2
integer :: i
class(t), allocatable :: x(:)

allocate(t2 :: x(10))
select type(x)
 type is(t2)
  if (size (x) /= 10) call abort ()
  x = [(t2(a=-i, B=[1*i,2*i,3*i,4*i]), i = 1, 10)]
  do i = 1, 10
    if (x(i)%a /= -i .or. size (x(i)%b) /= 4 &
        .or. any (x(i)%b /= [1*i,2*i,3*i,4*i])) then
        call abort()
    end if
    if (x(i)%z /= cmplx(3.3, 4.4)) call abort()
  end do
  class default
    call abort()
end select

call base(x)
call baseExplicit(x, size(x))
call class(x)
call classExplicit(x, size(x))
contains
  subroutine base(y)
    type(t) :: y(:)
    if (size (y) /= 10) call abort ()
    do i = 1, 10
      if (y(i)%a /= -i .or. size (y(i)%b) /= 4 &
          .or. any (y(i)%b /= [1*i,2*i,3*i,4*i])) then
        call abort()
      end if
    end do
  end subroutine base
  subroutine baseExplicit(v, n)
    integer, intent(in) :: n
    type(t) :: v(n)
    if (size (v) /= 10) call abort ()
    do i = 1, 10
      if (v(i)%a /= -i .or. size (v(i)%b) /= 4 &
          .or. any (v(i)%b /= [1*i,2*i,3*i,4*i])) then
        call abort()
      end if
    end do
  end subroutine baseExplicit
  subroutine class(z)
    class(t), intent(in) :: z(:)
    select type(z)
     type is(t2)
      if (size (z) /= 10) call abort ()
      do i = 1, 10
        if (z(i)%a /= -i .or. size (z(i)%b) /= 4 &
            .or. any (z(i)%b /= [1*i,2*i,3*i,4*i])) then
            call abort()
        end if
        if (z(i)%z /= cmplx(3.3, 4.4)) call abort()
      end do
      class default
        call abort()
    end select
    call base(z)
    call baseExplicit(z, size(z))
  end subroutine class
  subroutine classExplicit(u, n)
    integer, intent(in) :: n
    class(t), intent(in) :: u(n)
    select type(u)
     type is(t2)
      if (size (u) /= 10) call abort ()
      do i = 1, 10
        if (u(i)%a /= -i .or. size (u(i)%b) /= 4 &
            .or. any (u(i)%b /= [1*i,2*i,3*i,4*i])) then
            call abort()
        end if
        if (u(i)%z /= cmplx(3.3, 4.4)) call abort()
      end do
      class default
        call abort()
    end select
    call base(u)
    call baseExplicit(u, n)
  end subroutine classExplicit
end

