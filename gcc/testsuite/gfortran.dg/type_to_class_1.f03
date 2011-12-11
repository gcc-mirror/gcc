! { dg-do run }
!
! Passing TYPE to CLASS
!
implicit none
type t
  integer :: A
  real, allocatable :: B(:)
end type t

type(t), allocatable :: x(:)
type(t) :: y(10)
integer :: i

allocate(x(10))
if (size (x) /= 10) call abort ()
x = [(t(a=-i, B=[1*i,2*i,3*i,4*i]), i = 1, 10)]
do i = 1, 10
  if (x(i)%a /= -i .or. size (x(i)%b) /= 4 &
      .or. any (x(i)%b /= [1*i,2*i,3*i,4*i])) then
      call abort()
  end if
end do

y = x ! TODO: Segfaults in runtime without 'y' being set

call class(x)
call classExplicit(x, size(x))
call class(y)
call classExplicit(y, size(y))

contains
  subroutine class(z)
    class(t), intent(in) :: z(:)
    select type(z)
     type is(t)
      if (size (z) /= 10) call abort ()
      do i = 1, 10
        if (z(i)%a /= -i .or. size (z(i)%b) /= 4 &
            .or. any (z(i)%b /= [1*i,2*i,3*i,4*i])) then
            call abort()
        end if
      end do
      class default
        call abort()
    end select
  end subroutine class
  subroutine classExplicit(u, n)
    integer, intent(in) :: n
    class(t), intent(in) :: u(n)
    select type(u)
     type is(t)
      if (size (u) /= 10) call abort ()
      do i = 1, 10
        if (u(i)%a /= -i .or. size (u(i)%b) /= 4 &
            .or. any (u(i)%b /= [1*i,2*i,3*i,4*i])) then
            call abort()
        end if
      end do
      class default
        call abort()
    end select
  end subroutine classExplicit
end

