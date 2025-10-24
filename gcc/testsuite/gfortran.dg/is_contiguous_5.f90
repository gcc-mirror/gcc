! { dg-do run }
! PR fortran/114023 - IS_CONTIGUOUS and pointers to non-contiguous targets
!
! Based on testcase by Federico Perini

program main
  implicit none
  complex, parameter :: cvals(*) = [(1,-1),(2,-2),(3,-3)]
  complex             , target :: cref(size(cvals)) = cvals  ! Reference
  complex, allocatable, target :: carr(:)                    ! Test

  type cx
     real :: re, im
  end type cx
  type(cx), parameter :: tvals(*)  = [cx(1,-1),cx(2,-2),cx(3,-3)]
  real, parameter     :: expect(*) = tvals% re
  type(cx)             , target :: tref(size(cvals)) = tvals ! Reference
  type(cx), allocatable, target :: tarr(:)

  real,     pointer  :: rr1(:), rr2(:), rr3(:), rr4(:)
  class(*), pointer  :: cp1(:), cp2(:), cp3(:), cp4(:)

  carr = cvals
  tarr = tvals

  if (any (expect /= [1,2,3])) error stop 90

  ! REAL pointer to non-contiguous effective target
  rr1(1:3) => cref%re
  rr2      => cref%re
  rr3(1:3) => carr%re
  rr4      => carr%re

  if (is_contiguous      (rr1))          stop 1
  if (my_contiguous_real (rr1))          stop 2
  if (is_contiguous      (cref(1:3)%re)) stop 3
! if (my_contiguous_real (cref(1:3)%re)) stop 4     ! pr122397

  if (is_contiguous      (rr3))          stop 6
  if (my_contiguous_real (rr3))          stop 7
  if (is_contiguous      (carr(1:3)%re)) stop 8
! if (my_contiguous_real (carr(1:3)%re)) stop 9

  if (is_contiguous      (rr2))     stop 11
  if (my_contiguous_real (rr2))     stop 12
  if (is_contiguous      (cref%re)) stop 13
! if (my_contiguous_real (cref%re)) stop 14

  if (is_contiguous      (rr4))     stop 16
  if (my_contiguous_real (rr4))     stop 17
  if (is_contiguous      (carr%re)) stop 18
! if (my_contiguous_real (carr%re)) stop 19

  rr1(1:3) => tref%re
  rr2      => tref%re
  rr3(1:3) => tarr%re
  rr4      => tarr%re

  if (is_contiguous      (rr1))          stop 21
  if (my_contiguous_real (rr1))          stop 22
  if (is_contiguous      (tref(1:3)%re)) stop 23
! if (my_contiguous_real (tref(1:3)%re)) stop 24

  if (is_contiguous      (rr3))          stop 26
  if (my_contiguous_real (rr3))          stop 27
  if (is_contiguous      (tarr(1:3)%re)) stop 28
! if (my_contiguous_real (tarr(1:3)%re)) stop 29

  if (is_contiguous      (rr2))     stop 31
  if (my_contiguous_real (rr2))     stop 32
  if (is_contiguous      (tref%re)) stop 33
! if (my_contiguous_real (tref%re)) stop 34

  if (is_contiguous      (rr4))     stop 36
  if (my_contiguous_real (rr4))     stop 37
  if (is_contiguous      (tarr%re)) stop 38
! if (my_contiguous_real (tarr%re)) stop 39

  ! Unlimited polymorphic pointer to non-contiguous effective target
  cp1(1:3) => cref%re
  cp2      => cref%re
  cp3(1:3) => carr%re
  cp4      => carr%re

  if (is_contiguous      (cp1)) stop 41
  if (my_contiguous_poly (cp1)) stop 42
  if (is_contiguous      (cp2)) stop 43
  if (my_contiguous_poly (cp2)) stop 44
  if (is_contiguous      (cp3)) stop 45
  if (my_contiguous_poly (cp3)) stop 46
  if (is_contiguous      (cp4)) stop 47
  if (my_contiguous_poly (cp4)) stop 48

  cp1(1:3) => tref%re
  cp2      => tref%re
  cp3(1:3) => tarr%re
  cp4      => tarr%re

  if (is_contiguous      (cp1)) stop 51
  if (my_contiguous_poly (cp1)) stop 52
  if (is_contiguous      (cp2)) stop 53
  if (my_contiguous_poly (cp2)) stop 54
  if (is_contiguous      (cp3)) stop 55
  if (my_contiguous_poly (cp3)) stop 56
  if (is_contiguous      (cp4)) stop 57
  if (my_contiguous_poly (cp4)) stop 58

  deallocate (carr, tarr)
contains
  pure logical function my_contiguous_real (x) result (res)
    real, pointer, intent(in) :: x(:)
    res = is_contiguous (x)
    if (any (x /= expect)) error stop 97
  end function my_contiguous_real

  pure logical function my_contiguous_poly (x) result (res)
    class(*), pointer, intent(in) :: x(:)
    res = is_contiguous (x)
    select type (x)
    type is (real)
       if (any (x /= expect)) error stop 98
    class default
       error stop 99
    end select
  end function my_contiguous_poly
end
