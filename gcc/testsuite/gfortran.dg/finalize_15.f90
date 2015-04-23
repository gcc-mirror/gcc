! { dg-do run }
!
! PR fortran/37336
!
! Check the scalarizer/array packing with strides
! in the finalization wrapper
!
module m
  implicit none

  type t1
    integer :: i = 1
  contains
    final :: fini_elem
  end type t1

  type, extends(t1) :: t1e
    integer :: j = 11
  contains
    final :: fini_elem2
  end type t1e

  type t2
    integer :: i = 2
  contains
    final :: fini_shape
  end type t2

  type, extends(t2) :: t2e
    integer :: j = 22
  contains
    final :: fini_shape2
  end type t2e

  type t3
    integer :: i = 3
  contains
    final :: fini_explicit
  end type t3

  type, extends(t3) :: t3e
    integer :: j = 33
  contains
    final :: fini_explicit2
  end type t3e

  integer :: cnt1, cnt1e, cnt2, cnt2e, cnt3, cnt3e

contains

  impure elemental subroutine fini_elem(x)
    type(t1), intent(inout) :: x
    integer :: i, j, i2, j2

    if (cnt1e /= 5*4) call abort ()
    j = mod (cnt1,5)+1
    i = cnt1/5 + 1
    i2 = (i-1)*3 + 1
    j2 = (j-1)*2 + 1
    if (x%i /= j2 + 100*i2) call abort ()
    x%i = x%i * (-13)
    cnt1 = cnt1 + 1
  end subroutine fini_elem

  impure elemental subroutine fini_elem2(x)
    type(t1e), intent(inout) :: x
    integer :: i, j, i2, j2

    j = mod (cnt1e,5)+1
    i = cnt1e/5 + 1
    i2 = (i-1)*3 + 1
    j2 = (j-1)*2 + 1
    if (x%i /= j2 + 100*i2) call abort ()
    if (x%j /= (j2 + 100*i2)*100) call abort ()
    x%j = x%j * (-13)
    cnt1e = cnt1e + 1
  end subroutine fini_elem2

  subroutine fini_shape(x)
    type(t2) :: x(:,:)
    if (cnt2e /= 1 .or. cnt2 /= 0) call abort ()
    call check_var_sec(x%i, 1)
    x%i = x%i * (-13)
    cnt2 = cnt2 + 1
  end subroutine fini_shape

  subroutine fini_shape2(x)
    type(t2e) :: x(:,:)
    call check_var_sec(x%i, 1)
    call check_var_sec(x%j, 100)
    x%j = x%j * (-13)
    cnt2e = cnt2e + 1
  end subroutine fini_shape2

  subroutine fini_explicit(x)
    type(t3) :: x(5,4)
    if (cnt3e /= 1 .or. cnt3 /= 0) call abort ()
    call check_var_sec(x%i, 1)
    x%i = x%i * (-13)
    cnt3 = cnt3 + 1
  end subroutine fini_explicit

  subroutine fini_explicit2(x)
    type(t3e) :: x(5,4)
    call check_var_sec(x%i, 1)
    call check_var_sec(x%j, 100)
    x%j = x%j * (-13)
    cnt3e = cnt3e + 1
  end subroutine fini_explicit2

  subroutine fin_test_1(x)
    class(t1), intent(out) :: x(5,4)
  end subroutine fin_test_1

  subroutine fin_test_2(x)
    class(t2), intent(out) :: x(:,:)
  end subroutine fin_test_2

  subroutine fin_test_3(x)
    class(t3), intent(out) :: x(:,:)
    if (any (shape(x) /= [5,4])) call abort ()
  end subroutine fin_test_3

  subroutine check_var_sec(x, factor)
    integer :: x(:,:)
    integer, value :: factor
    integer :: i, j, i2, j2

    do i = 1, 4
      i2 = (i-1)*3 + 1
      do j = 1, 5
        j2 = (j-1)*2 + 1
        if (x(j,i) /= (j2 + 100*i2)*factor) call abort ()
      end do
    end do
  end subroutine check_var_sec
end module m


program test
  use m
  implicit none

  class(t1), allocatable :: x(:,:)
  class(t2), allocatable :: y(:,:)
  class(t3), allocatable :: z(:,:)
  integer :: i, j

  cnt1 = 0; cnt1e = 0; cnt2 = 0; cnt2e = 0;  cnt3 = 0; cnt3e = 0

  allocate (t1e :: x(10,10))
  allocate (t2e :: y(10,10))
  allocate (t3e :: z(10,10))

  select type(x)
    type is (t1e)
      do i = 1, 10
        do j = 1, 10
          x(j,i)%i = j + 100*i
          x(j,i)%j = (j + 100*i)*100
        end do
      end do
  end select

  select type(y)
    type is (t2e)
      do i = 1, 10
        do j = 1, 10
          y(j,i)%i = j + 100*i
          y(j,i)%j = (j + 100*i)*100
        end do
      end do
  end select

  select type(z)
    type is (t3e)
      do i = 1, 10
        do j = 1, 10
          z(j,i)%i = j + 100*i
          z(j,i)%j = (j + 100*i)*100
        end do
      end do
  end select

  if (cnt1 + cnt1e + cnt2 + cnt2e + cnt3 + cnt3e /= 0) call abort()

  call fin_test_1(x(::2,::3))
  if (cnt1 /= 5*4) call abort ()
  if (cnt1e /= 5*4) call abort ()
  cnt1 = 0; cnt1e = 0
  if (cnt2 + cnt2e + cnt3 + cnt3e /= 0) call abort()

  call fin_test_2(y(::2,::3))
  if (cnt2 /= 1) call abort ()
  if (cnt2e /= 1) call abort ()
  cnt2 = 0; cnt2e = 0
  if (cnt1 + cnt1e + cnt3 + cnt3e /= 0) call abort()

  call fin_test_3(z(::2,::3))
  if (cnt3 /= 1) call abort ()
  if (cnt3e /= 1) call abort ()
  cnt3 = 0; cnt3e = 0
  if (cnt1 + cnt1e + cnt2 + cnt2e /= 0) call abort()

  select type(x)
    type is (t1e)
      call check_val(x%i, 1, 1)
      call check_val(x%j, 100, 11)
  end select

  select type(y)
    type is (t2e)
      call check_val(y%i, 1, 2)
      call check_val(y%j, 100, 22)
  end select

  select type(z)
    type is (t3e)
      call check_val(z%i, 1, 3)
      call check_val(z%j, 100, 33)
  end select

contains
  subroutine check_val(x, factor, val)
    integer :: x(:,:)
    integer, value :: factor, val
    integer :: i, j
    do i = 1, 10
      do j = 1, 10
        if (mod (j-1, 2) == 0 .and. mod (i-1, 3) == 0) then
          if (x(j,i) /= val) call abort ()
        else
          if (x(j,i) /= (j + 100*i)*factor) call abort ()
        end if
      end do
    end do
  end subroutine check_val
end program test
