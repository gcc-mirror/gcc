! { dg-do run }
!
! Tests fix for PR100120
!

program main_p

  implicit none

  integer, parameter :: n = 11
  integer, parameter :: m = 7
  integer, parameter :: c = 63

  type :: foo_t
    integer :: i
  end type foo_t

  type, extends(foo_t) :: bar_t
    integer :: j(n)
  end type bar_t

  integer,          target :: ain(n)
  character,        target :: ac1(n)
  character(len=m), target :: acn(n)
  type(foo_t),      target :: afd(n)
  type(bar_t),      target :: abd(n)
  !
  class(foo_t),    pointer :: spf
  class(foo_t),    pointer :: apf(:)
  class(bar_t),    pointer :: spb
  class(bar_t),    pointer :: apb(:)
  class(*),        pointer :: spu
  class(*),        pointer :: apu(:)
  integer                  :: i, j

  ain = [(i, i=1,n)]
  ac1 = [(achar(i+c), i=1,n)]
  do i = 1, n
    do j = 1, m
      acn(i)(j:j) = achar(i*m+j+c-m)
    end do
  end do
  afd%i = ain
  abd%i = ain
  do i = 1, n
    abd(i)%j = 2*i*ain
  end do
  !
  spf => afd(n)
  if(.not.associated(spf))         stop 1
  if(.not.associated(spf, afd(n))) stop 2
  if(spf%i/=n)                     stop 3
  apf => afd
  if(.not.associated(apf))         stop 4
  if(.not.associated(apf, afd))    stop 5
  if(any(apf%i/=afd%i))            stop 6
  !
  spf => abd(n)
  if(.not.associated(spf))         stop 7
  if(.not.associated(spf, abd(n))) stop 8
  if(spf%i/=n)                     stop 9
  select type(spf)
  type is(bar_t)
    if(any(spf%j/=2*n*ain))        stop 10
  class default
    stop 11
  end select
  apf => abd
  if(.not.associated(apf))         stop 12
  if(.not.associated(apf, abd))    stop 13
  if(any(apf%i/=abd%i))            stop 14
  select type(apf)
  type is(bar_t)
    do i = 1, n
      if(any(apf(i)%j/=2*i*ain))   stop 15
    end do
  class default
    stop 16
  end select
  !
  spb => abd(n)
  if(.not.associated(spb))         stop 17
  if(.not.associated(spb, abd(n))) stop 18
  if(spb%i/=n)                     stop 19
  if(any(spb%j/=2*n*ain))          stop 20
  apb => abd
  if(.not.associated(apb))         stop 21
  if(.not.associated(apb, abd))    stop 22
  if(any(apb%i/=abd%i))            stop 23
  do i = 1, n
    if(any(apb(i)%j/=2*i*ain))     stop 24
  end do
  !
  spu => ain(n)
  if(.not.associated(spu))         stop 25
  if(.not.associated(spu, ain(n))) stop 26
  select type(spu)
  type is(integer)
    if(spu/=n)                     stop 27
  class default
    stop 28
  end select
  apu => ain
  if(.not.associated(apu))         stop 29
  if(.not.associated(apu, ain))    stop 30
  select type(apu)
  type is(integer)
    if(any(apu/=ain))              stop 31
  class default
    stop 32
  end select
  !
  spu => ac1(n)
  if(.not.associated(spu))         stop 33
  if(.not.associated(spu, ac1(n))) stop 34
  select type(spu)
  type is(character(len=*))
    if(len(spu)/=1)                stop 35
    if(spu/=ac1(n))                stop 36
  class default
    stop 37
  end select
  apu => ac1
  if(.not.associated(apu))         stop 38
  if(.not.associated(apu, ac1))    stop 39
  select type(apu)
  type is(character(len=*))
    if(len(apu)/=1)                stop 40
    if(any(apu/=ac1))              stop 41
  class default
    stop 42
  end select
  !
  spu => acn(n)
  if(.not.associated(spu))         stop 43
  if(.not.associated(spu, acn(n))) stop 44
  select type(spu)
  type is(character(len=*))
    if(len(spu)/=m)                stop 45
    if(spu/=acn(n))                stop 46
  class default
    stop 47
  end select
  apu => acn
  if(.not.associated(apu))         stop 48
  if(.not.associated(apu, acn))    stop 49
  select type(apu)
  type is(character(len=*))
    if(len(apu)/=m)                stop 50
    if(any(apu/=acn))              stop 51
  class default
    stop 52
  end select
  !
  spu => afd(n)
  if(.not.associated(spu))         stop 53
  if(.not.associated(spu, afd(n))) stop 54
  select type(spu)
  type is(foo_t)
    if(spu%i/=n)                   stop 55
  class default
    stop 56
  end select
  apu => afd
  if(.not.associated(apu))         stop 57
  if(.not.associated(apu, afd))    stop 58
  select type(apu)
  type is(foo_t)
    if(any(apu%i/=afd%i))          stop 59
  class default
    stop 60
  end select
  !
  spu => abd(n)
  if(.not.associated(spu))         stop 61
  if(.not.associated(spu, abd(n))) stop 62
  select type(spu)
  type is(bar_t)
    if(spu%i/=n)                   stop 63
    if(any(spu%j/=2*n*ain))        stop 64
  class default
    stop 65
  end select
  apu => abd
  if(.not.associated(apu))         stop 66
  if(.not.associated(apu, abd))    stop 67
  select type(apu)
  type is(bar_t)
    if(any(apu%i/=abd%i))          stop 68
    do i = 1, n
      if(any(apu(i)%j/=2*i*ain))   stop 69
    end do
  class default
    stop 70
  end select
  stop

end program main_p
