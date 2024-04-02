! { dg-do run }
! Test of an issue found in the investigation of PR112407
! Contributed by Tomas Trnka  <trnka@scm.com>
!
module m
  private new_t

  type s
    procedure(),pointer,nopass :: op
  end type

  type :: t
    integer :: i
    type (s) :: s
  contains
    procedure :: new_t
    procedure :: bar
    procedure :: add_t
    generic :: new => new_t, bar
    generic, public :: assignment(=) => add_t
    final :: final_t
  end type

  integer :: i = 0, finals = 0

contains
  recursive subroutine new_t (arg1, arg2)
    class(t), intent(out) :: arg1
    type(t), intent(in)  :: arg2
    i = i + 1

    print "(a,2i4)", "new_t", arg1%i, arg2%i
    if (i .ge. 10) return

! According to F2018(8.5.10), arg1 should be undefined on invocation, unless
! any sub-components are default initialised. gfc used to set arg1%i = 0.
    if (arg1%i .ne. arg2%i) then
      arg1%i = arg2%i
      call arg1%new(arg2)
    endif
  end

  subroutine bar(arg)
    class(t), intent(out) :: arg
    call arg%new(t(42, s(new_t)))
  end

  subroutine add_t (arg1, arg2)
    class(t), intent(out) :: arg1
    type(t), intent(in)  :: arg2
    call arg1%new (arg2)
  end

  impure elemental subroutine final_t (arg1)
    type(t), intent(in) :: arg1
    finals = finals + 1
  end
end

  use m
  class(t), allocatable :: x
  allocate(x)
  x%i = 0
  call x%new()                   ! gfortran used to output 10*'new_t'
  print "(3i4)", x%i, i, finals  !           -||-          0 10 11
!
! The other brands output 2*'new_t' + 42 2 3 and now so does gfc :-)
  if (x%i .ne. 42) stop 1
  if (i .ne. 2) stop 2
  if (finals .ne. 3) stop 3
end
