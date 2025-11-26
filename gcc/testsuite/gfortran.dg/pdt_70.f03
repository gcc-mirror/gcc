! { dg-do run }
!
! PR104650
! Contributed by Gerhard Steinmetz  <gscfq@t-online.de>
!
module m1
   type t1
      integer :: i
   contains
      final :: s
   end type
   type t2(n)
      integer, len :: n = 1
      type(t1) :: a
   end type
   integer :: ctr = 0

contains

   impure elemental subroutine s(x)
      type(t1), intent(in) :: x
      ctr = ctr + x%i
   end
end

! From F2018: C.2.6 Final subroutines (7.5.6, 7.5.6.2, 7.5.6.3, 7.5.6.4)
module m2

  type t(k)
    integer, kind :: k
    real(k), pointer :: vector(:) => NULL ()
  contains
    final :: finalize_t1s, finalize_t1v, finalize_t2e
  end type

  integer :: flag = 0

contains

  impure subroutine finalize_t1s(x)
    type(t(kind(0.0))) x
    if (associated(x%vector)) deallocate(x%vector)
    flag = flag + 1
  END subroutine

  impure subroutine finalize_t1v(x)
    type(t(kind(0.0))) x(:)
    do i = lbound(x,1), ubound(x,1)
      if (associated(x(i)%vector)) deallocate(x(i)%vector)
      flag = flag + 1
    end do
  end subroutine

  impure elemental subroutine finalize_t2e(x)
    type(t(kind(0.0d0))), intent(inout) :: x
    if (associated(x%vector)) deallocate(x%vector)
    flag = flag + 1
  end subroutine

  elemental subroutine alloc_ts (x)
    type(t(kind(0.0))), intent(inout) :: x
    allocate (x%vector, source = [42.0,-42.0])
  end subroutine

  elemental subroutine alloc_td (x)
    type(t(kind(0.0d0))), intent(inout) :: x
    allocate (x%vector, source = [42.0d0,-42.0d0])
  end subroutine

end module

  use m1
  use m2
  integer, parameter :: dims = 2
  integer :: p = 42

! Test pr104650
  call u (kind(0e0), p)
  if (ctr /= p * (1 + kind(0e0))) stop 1

! Test the standard example
  call example (dims)
  if (flag /= 11 + dims**2) stop 2

contains

  subroutine u (k, p)
    integer :: k, p
    type (t2(k)) :: u_k, v_k(k)
    u_k%a%i = p
    v_k%a%i = p
  end

! Returning from 'example' will effectively do
!    call finalize_t1s(a)
!    call finalize_t1v(b)
!    call finalize_t2e(d)
! No final subroutine will be called for variable C because the user
! omitted to define a suitable specific procedure for it.
  subroutine example(n)
  type(t(kind(0.0))) a, b(10), c(n,2)
  type(t(kind(0.0d0))) d(n,n)
  real(kind(0.0)),target :: tgt(1)

  ! Explicit allocation to provide a valid memory refence for deallocation.
  call alloc_ts(a)
  call alloc_ts(b)
  call alloc_ts(c)
  call alloc_td(d)
  end subroutine

end
