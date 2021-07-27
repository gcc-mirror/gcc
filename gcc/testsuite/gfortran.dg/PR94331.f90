! { dg-do run }
! { dg-additional-sources PR94331.c }
!
! Test the fix for PR94331
!

program main_p
  
  use, intrinsic :: iso_c_binding, only: &
    c_int

  implicit none

  integer            :: i
  integer, parameter :: ex = 11
  integer, parameter :: lb = 11
  integer, parameter :: ub = ex+lb-1
  integer, parameter :: u(*) = [(i, i=1,ex)]
  
  interface
    function checkb_p_as(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int), pointer, intent(in) :: a(:)
      integer(kind=c_int),   value, intent(in) :: l
      integer(kind=c_int),   value, intent(in) :: u
      logical(kind=c_bool)                     :: c
    end function checkb_p_as
    function checkb_a_as(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int), allocatable, intent(in) :: a(:)
      integer(kind=c_int),       value, intent(in) :: l
      integer(kind=c_int),       value, intent(in) :: u
      logical(kind=c_bool)                         :: c
    end function checkb_a_as
    function checkb_o_as(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int),        intent(in) :: a(:)
      integer(kind=c_int), value, intent(in) :: l
      integer(kind=c_int), value, intent(in) :: u
      logical(kind=c_bool)                   :: c
    end function checkb_o_as
    function checkb_p_ar(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int), pointer, intent(in) :: a(..)
      integer(kind=c_int),   value, intent(in) :: l
      integer(kind=c_int),   value, intent(in) :: u
      logical(kind=c_bool)                     :: c
    end function checkb_p_ar
    function checkb_a_ar(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int), allocatable, intent(in) :: a(..)
      integer(kind=c_int),       value, intent(in) :: l
      integer(kind=c_int),       value, intent(in) :: u
      logical(kind=c_bool)                         :: c
    end function checkb_a_ar
    function checkb_o_ar(a, l, u) result(c) &
      bind(c, name="check_bounds")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool
      implicit none
      integer(kind=c_int),        intent(in) :: a(..)
      integer(kind=c_int), value, intent(in) :: l
      integer(kind=c_int), value, intent(in) :: u
      logical(kind=c_bool)                   :: c
    end function checkb_o_ar
  end interface

  integer(kind=c_int),              target :: a(lb:ub)
  integer(kind=c_int), allocatable, target :: b(:)
  integer(kind=c_int),             pointer :: p(:)

  a = u
  if(lbound(a,1)/=lb)             stop 1
  if(ubound(a,1)/=ub)             stop 2
  if(any(shape(a)/=[ex]))         stop 3
  if(.not.checkb_p_as(a, lb, ub)) stop 4
  if(lbound(a,1)/=lb)             stop 5
  if(ubound(a,1)/=ub)             stop 6
  if(any(shape(a)/=[ex]))         stop 7
  if(any(a/=u))                   stop 8
  !
  a = u
  if(lbound(a,1)/=lb)             stop 9
  if(ubound(a,1)/=ub)             stop 10
  if(any(shape(a)/=[ex]))         stop 11
  if(.not.checkb_p_ar(a, lb, ub)) stop 12
  if(lbound(a,1)/=lb)             stop 13
  if(ubound(a,1)/=ub)             stop 14
  if(any(shape(a)/=[ex]))         stop 15
  if(any(a/=u))                   stop 16
  !
  a = u
  if(lbound(a,1)/=lb)             stop 17
  if(ubound(a,1)/=ub)             stop 18
  if(any(shape(a)/=[ex]))         stop 19
  if(.not.checkb_o_as(a, 0, ex-1))stop 20
  if(lbound(a,1)/=lb)             stop 21
  if(ubound(a,1)/=ub)             stop 22
  if(any(shape(a)/=[ex]))         stop 23
  if(any(a/=u))                   stop 24
  !
  a = u
  if(lbound(a,1)/=lb)             stop 25
  if(ubound(a,1)/=ub)             stop 26
  if(any(shape(a)/=[ex]))         stop 27
  if(.not.checkb_o_ar(a, 0, ex-1))stop 28
  if(lbound(a,1)/=lb)             stop 29
  if(ubound(a,1)/=ub)             stop 30
  if(any(shape(a)/=[ex]))         stop 31
  if(any(a/=u))                   stop 32
  !
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 33
  if(ubound(b,1)/=ub)             stop 34
  if(any(shape(b)/=[ex]))         stop 35
  if(.not.checkb_p_as(b, lb, ub)) stop 36
  if(.not.allocated(b))           stop 37
  if(lbound(b,1)/=lb)             stop 38
  if(ubound(b,1)/=ub)             stop 39
  if(any(shape(b)/=[ex]))         stop 40
  if(any(b/=u))                   stop 41
  !
  deallocate(b)
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 42
  if(ubound(b,1)/=ub)             stop 43
  if(any(shape(b)/=[ex]))         stop 44
  if(.not.checkb_p_ar(b, lb, ub)) stop 45
  if(.not.allocated(b))           stop 46
  if(lbound(b,1)/=lb)             stop 47
  if(ubound(b,1)/=ub)             stop 48
  if(any(shape(b)/=[ex]))         stop 49
  if(any(b/=u))                   stop 50
  !
  deallocate(b)
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 51
  if(ubound(b,1)/=ub)             stop 52
  if(any(shape(b)/=[ex]))         stop 53
  if(.not.checkb_a_as(b, lb, ub)) stop 54
  if(.not.allocated(b))           stop 55
  if(lbound(b,1)/=lb)             stop 56
  if(ubound(b,1)/=ub)             stop 57
  if(any(shape(b)/=[ex]))         stop 58
  if(any(b/=u))                   stop 59
  !
  deallocate(b)
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 60
  if(ubound(b,1)/=ub)             stop 61
  if(any(shape(b)/=[ex]))         stop 62
  if(.not.checkb_a_ar(b, lb, ub)) stop 63
  if(.not.allocated(b))           stop 64
  if(lbound(b,1)/=lb)             stop 65
  if(ubound(b,1)/=ub)             stop 66
  if(any(shape(b)/=[ex]))         stop 67
  if(any(b/=u))                   stop 68
  !
  deallocate(b)
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 69
  if(ubound(b,1)/=ub)             stop 70
  if(any(shape(b)/=[ex]))         stop 71
  if(.not.checkb_o_as(b, 0, ex-1))stop 72
  if(.not.allocated(b))           stop 73
  if(lbound(b,1)/=lb)             stop 74
  if(ubound(b,1)/=ub)             stop 75
  if(any(shape(b)/=[ex]))         stop 76
  if(any(b/=u))                   stop 77
  !
  deallocate(b)
  allocate(b(lb:ub), source=u)
  if(lbound(b,1)/=lb)             stop 78
  if(ubound(b,1)/=ub)             stop 79
  if(any(shape(b)/=[ex]))         stop 80
  if(.not.checkb_o_ar(b, 0, ex-1))stop 81
  if(.not.allocated(b))           stop 82
  if(lbound(b,1)/=lb)             stop 83
  if(ubound(b,1)/=ub)             stop 84
  if(any(shape(b)/=[ex]))         stop 85
  if(any(b/=u))                   stop 86
  deallocate(b)
  !
  p(lb:ub) => a
  if(lbound(p,1)/=lb)             stop 87
  if(ubound(p,1)/=ub)             stop 88
  if(any(shape(p)/=[ex]))         stop 89
  if(.not.checkb_p_as(p, lb, ub)) stop 90
  if(.not.associated(p))          stop 91
  if(.not.associated(p, a))       stop 92
  if(lbound(p,1)/=lb)             stop 93
  if(ubound(p,1)/=ub)             stop 94
  if(any(shape(p)/=[ex]))         stop 95
  if(any(p/=u))                   stop 96
  !
  nullify(p)
  p(lb:ub) => a
  if(lbound(p,1)/=lb)             stop 97
  if(ubound(p,1)/=ub)             stop 98
  if(any(shape(p)/=[ex]))         stop 99
  if(.not.checkb_p_ar(p, lb, ub)) stop 100
  if(.not.associated(p))          stop 101
  if(.not.associated(p, a))       stop 102
  if(lbound(p,1)/=lb)             stop 103
  if(ubound(p,1)/=ub)             stop 104
  if(any(shape(p)/=[ex]))         stop 105
  if(any(p/=u))                   stop 106
  !
  nullify(p)
  p(lb:ub) => a
  if(lbound(p,1)/=lb)             stop 107
  if(ubound(p,1)/=ub)             stop 108
  if(any(shape(p)/=[ex]))         stop 109
  if(.not.checkb_o_as(p, 0, ex-1))stop 110
  if(.not.associated(p))          stop 111
  if(.not.associated(p, a))       stop 112
  if(lbound(p,1)/=lb)             stop 113
  if(ubound(p,1)/=ub)             stop 114
  if(any(shape(p)/=[ex]))         stop 115
  if(any(p/=u))                   stop 116
  !
  nullify(p)
  p(lb:ub) => a
  if(lbound(p,1)/=lb)             stop 117
  if(ubound(p,1)/=ub)             stop 118
  if(any(shape(p)/=[ex]))         stop 119
  if(.not.checkb_o_ar(p, 0, ex-1))stop 120
  if(.not.associated(p))          stop 121
  if(.not.associated(p, a))       stop 122
  if(lbound(p,1)/=lb)             stop 123
  if(ubound(p,1)/=ub)             stop 124
  if(any(shape(p)/=[ex]))         stop 125
  if(any(p/=u))                   stop 126
  nullify(p)
  stop
  
end program main_p
