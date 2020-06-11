! { dg-do run }
!
! Test the fix for PR94022
!

function isasa_f(a) result(s)
  implicit none

  integer, intent(in) :: a(..)
  
  logical :: s
  
  select rank(a)
  rank(*)
    s = .true.
  rank default
    s = .false.
  end select
  return
end function isasa_f

function isasa_c(a) result(s) bind(c)
  use, intrinsic :: iso_c_binding, only: c_int, c_bool

  implicit none

  integer(kind=c_int), intent(in) :: a(..)
  
  logical(kind=c_bool) :: s
  
  select rank(a)
  rank(*)
    s = .true.
  rank default
    s = .false.
  end select
  return
end function isasa_c

program isasa_p

  implicit none

  interface
    function isasa_f(a) result(s)
      implicit none
      integer, intent(in) :: a(..)
      logical             :: s
    end function isasa_f
    function isasa_c(a) result(s) bind(c)
      use, intrinsic :: iso_c_binding, only: c_int, c_bool
      implicit none
      integer(kind=c_int), intent(in) :: a(..)
      logical(kind=c_bool)            :: s
    end function isasa_c
  end interface

  integer, parameter :: sz = 7
  integer, parameter :: lb = 3
  integer, parameter :: ub = 9
  integer, parameter :: ex = ub-lb+1

  integer :: arr(sz,lb:ub)

  arr = 1
  if (asaf_a(arr, lb+1, ub-1)) stop 1
  if (asaf_p(arr, lb+1, ub-1)) stop 2
  if (asaf_a(arr, 2, ex-1))    stop 3
  if (asaf_p(arr, 2, ex-1))    stop 4
  if (asac_a(arr, lb+1, ub-1)) stop 5
  if (asac_p(arr, lb+1, ub-1)) stop 6
  if (asac_a(arr, 2, ex-1))    stop 7
  if (asac_p(arr, 2, ex-1))    stop 8
  
  stop

contains

  function asaf_a(a, lb, ub) result(s)
    integer, intent(in) :: lb
    integer, target, intent(in) :: a(sz,lb:*)
    integer, intent(in) :: ub

    logical :: s

    s = isasa_f(a(:,lb:ub))
    return
  end function asaf_a

  function asaf_p(a, lb, ub) result(s)
    integer,         intent(in) :: lb
    integer, target, intent(in) :: a(sz,lb:*)
    integer,         intent(in) :: ub

    logical :: s

    integer, pointer :: p(:,:)

    p => a(:,lb:ub)
    s = isasa_f(p)
    return
  end function asaf_p

  function asac_a(a, lb, ub) result(s)
    integer, intent(in) :: lb
    integer, target, intent(in) :: a(sz,lb:*)
    integer, intent(in) :: ub

    logical :: s

    s = logical(isasa_c(a(:,lb:ub)))
    return
  end function asac_a

  function asac_p(a, lb, ub) result(s)
    integer,         intent(in) :: lb
    integer, target, intent(in) :: a(sz,lb:*)
    integer,         intent(in) :: ub

    logical :: s

    integer, pointer :: p(:,:)

    p => a(:,lb:ub)
    s = logical(isasa_c(p))
    return
  end function asac_p

end program isasa_p


  
