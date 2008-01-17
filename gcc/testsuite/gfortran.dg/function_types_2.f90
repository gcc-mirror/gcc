! { dg-do compile }
! Tests the fix for PR34431 in which function TYPEs that were
! USE associated would cause an error.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
module m1
  integer :: hh
  type t
    real :: r
  end type t
end module m1

module m2
  type t
    integer :: k
  end type t
end module m2

module m3
contains
  type(t) function func()
    use m2
    func%k = 77
  end function func
end module m3

type(t) function a()
  use m1, only: hh
  type t2
    integer :: j
  end type t2
  type t
    logical :: b
  end type t

  a%b = .true.
end function a

type(t) function b()
  use m1, only: hh
  use m2
  use m3
  b = func ()
  b%k = 5
end function b

type(t) function c()
  use m1, only: hh
  type t2
    integer :: j
  end type t2
  type t
    logical :: b
  end type t

  c%b = .true.
end function c

program main
  type t
    integer :: m
  end type t
contains
  type(t) function a1()
    use m1, only: hh
    type t2
      integer :: j
    end type t2
    type t
      logical :: b
    end type t

    a1%b = .true.
  end function a1

  type(t) function b1()
    use m1, only: hh
    use m2, only: t
! NAG f95 believes that the host-associated type(t)
! should be used:
!   b1%m = 5
! However, I (Tobias Burnus) believe that the use-associated one should
! be used:
    b1%k = 5
  end function b1

  type(t) function c1()
    use m1, only: hh
    type t2
      integer :: j
    end type t2
    type t
      logical :: b
    end type t

    c1%b = .true.
  end function c1

  type(t) function d1()
    d1%m = 55
  end function d1
end program main
! { dg-final { cleanup-modules "m1 m2 m3" } }
