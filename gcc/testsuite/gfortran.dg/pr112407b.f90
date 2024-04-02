! { dg-do compile }
! { dg-options "-std=f2008" }
! Test of an issue found in the investigation of PR112407. The dg-option is
! set to avoid regression once the F2018 RECURSIVE by default in implemented.
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
  subroutine new_t (arg1, arg2)            ! gfortran didn't detect the recursion
    class(t), intent(out) :: arg1
    type(t), intent(in)  :: arg2
    i = i + 1

    print *, "new_t", arg1%i, arg2%i
    if (i .ge. 10) return

    if (arg1%i .ne. arg2%i) then
      arg1%i = arg2%i
      call arg1%new(arg2)  ! { dg-warning "possibly calling itself recursively" }
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
