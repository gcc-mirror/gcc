! { dg-do run }
! { dg-additional-sources PR94327.c }
!
! Test the fix for PR94327
!

program attr_p
  
  use, intrinsic :: iso_c_binding, only: &
    c_int, c_bool, c_char

  implicit none

  integer            :: i
  integer, parameter :: n = 11
  integer, parameter :: u(*) = [(i, i=1,n)]
  
  interface
    function attr_p_as(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int), pointer, intent(in) :: a(:)
      logical(kind=c_bool),  value, intent(in) :: s
      character(kind=c_char)                   :: c
    end function attr_p_as
    function attr_a_as(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int), allocatable, intent(in) :: a(:)
      logical(kind=c_bool),      value, intent(in) :: s
      character(kind=c_char)                       :: c
    end function attr_a_as
    function attr_o_as(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int),         intent(in) :: a(:)
      logical(kind=c_bool), value, intent(in) :: s
      character(kind=c_char)                  :: c
    end function attr_o_as
    function attr_p_ar(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int), pointer, intent(in) :: a(..)
      logical(kind=c_bool),  value, intent(in) :: s
      character(kind=c_char)                   :: c
    end function attr_p_ar
    function attr_a_ar(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int), allocatable, intent(in) :: a(..)
      logical(kind=c_bool),      value, intent(in) :: s
      character(kind=c_char)                       :: c
    end function attr_a_ar
    function attr_o_ar(a, s) result(c) &
      bind(c, name="get_attr")
      use, intrinsic :: iso_c_binding, only: &
        c_int, c_bool, c_char
      implicit none
      integer(kind=c_int),         intent(in) :: a(..)
      logical(kind=c_bool), value, intent(in) :: s
      character(kind=c_char)                  :: c
    end function attr_o_ar
  end interface

  integer(kind=c_int),              target :: a(n)
  integer(kind=c_int), allocatable, target :: b(:)
  integer(kind=c_int),             pointer :: p(:)
  character(kind=c_char)                   :: c

  a = u
  c = attr_p_as(a, .true._c_bool)
  if(c/='p')                stop 1
  if(any(a/=u))             stop 2
  !
  a = u
  c = attr_p_ar(a, .true._c_bool)
  if(c/='p')                stop 3
  if(any(a/=u))             stop 4
  !
  a = u
  c = attr_o_as(a, .true._c_bool)
  if(c/='o')                stop 5
  if(any(a/=u))             stop 6
  !
  a = u
  c = attr_o_ar(a, .true._c_bool)
  if(c/='o')                stop 7
  if(any(a/=u))             stop 8
  !
  allocate(b, source=u)
  c = attr_p_as(b, .true._c_bool)
  if(c/='p')                stop 9
  if(.not.allocated(b))     stop 10
  if(any(b/=u))             stop 11
  !
  deallocate(b)
  allocate(b, source=u)
  c = attr_p_ar(b, .true._c_bool)
  if(c/='p')                stop 12
  if(.not.allocated(b))     stop 13
  if(any(b/=u))             stop 14
  !
  deallocate(b)
  allocate(b, source=u)
  c = attr_a_as(b, .true._c_bool)
  if(c/='a')                stop 15
  if(.not.allocated(b))     stop 16
  if(any(b/=u))             stop 17
  !
  deallocate(b)
  allocate(b, source=u)
  c = attr_a_ar(b, .true._c_bool)
  if(c/='a')                stop 18
  if(.not.allocated(b))     stop 19
  if(any(b/=u))             stop 20
  !
  deallocate(b)
  allocate(b, source=u)
  c = attr_o_as(b, .true._c_bool)
  if(c/='o')                stop 21
  if(.not.allocated(b))     stop 22
  if(any(b/=u))             stop 23
  !
  deallocate(b)
  allocate(b, source=u)
  c = attr_o_ar(b, .true._c_bool)
  if(c/='o')                stop 24
  if(.not.allocated(b))     stop 25
  if(any(b/=u))             stop 26
  !
  deallocate(b)
  c = attr_a_as(b, .false._c_bool)
  if(c/='a')                stop 27
  if(allocated(b))          stop 28
  !
  c = attr_a_ar(b, .false._c_bool)
  if(c/='a')                stop 29
  if(allocated(b))          stop 30
  !
  nullify(p)
  p => a
  c = attr_p_as(p, .true._c_bool)
  if(c/='p')                stop 31
  if(.not.associated(p))    stop 32
  if(.not.associated(p, a)) stop 33
  if(any(p/=u))             stop 34
  !
  nullify(p)
  p => a
  c = attr_p_ar(p, .true._c_bool)
  if(c/='p')                stop 35
  if(.not.associated(p))    stop 36
  if(.not.associated(p, a)) stop 37
  if(any(p/=u))             stop 38
  !
  nullify(p)
  p => a
  c = attr_o_as(p, .true._c_bool)
  if(c/='o')                stop 39
  if(.not.associated(p))    stop 40
  if(.not.associated(p, a)) stop 41
  if(any(p/=u))             stop 42
  !
  nullify(p)
  p => a
  c = attr_o_ar(p, .true._c_bool)
  if(c/='o')                stop 43
  if(.not.associated(p))    stop 44
  if(.not.associated(p, a)) stop 45
  if(any(p/=u))             stop 46
  !
  nullify(p)
  c = attr_p_as(p, .false._c_bool)
  if(c/='p')                stop 47
  if(associated(p))         stop 48
  if(associated(p, a))      stop 49
  !
  nullify(p)
  c = attr_p_ar(p, .false._c_bool)
  if(c/='p')                stop 50
  if(associated(p))         stop 51
  if(associated(p, a))      stop 52
  stop

end program attr_p
