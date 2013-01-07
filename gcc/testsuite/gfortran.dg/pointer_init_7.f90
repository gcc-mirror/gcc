! { dg-do compile }
!
! PR fortran/55763
!

subroutine sub()
  type t
    integer :: i
  end type t

  type(t), target :: tgt
  type(t), target, save :: tgt2(2)

  type t2a
    type(t),  pointer :: cmp1 => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  end type t2a

  type t2b
    class(t), pointer :: cmp2 => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  end type t2b

  type t2c
    class(t), pointer :: cmp3 => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  end type t2c

  type t2d
    integer,  pointer :: cmp4 => tgt%i ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  end type t2d

  type(t),  pointer :: w => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  class(t), pointer :: x => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  class(*), pointer :: y => tgt   ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
  integer,  pointer :: z => tgt%i ! { dg-error "Pointer initialization target at .1. must have the SAVE attribute" }
end subroutine

program main
  type t3
    integer :: j
  end type t3

  type(t3), target :: tgt

  type t4
    type(t3),  pointer :: cmp1 => tgt   ! OK
    class(t3), pointer :: cmp2 => tgt   ! OK
    class(t3), pointer :: cmp3 => tgt   ! OK
    integer,   pointer :: cmp4 => tgt%j ! OK
  end type t4

  type(t3), target :: mytarget

  type(t3),  pointer :: a => mytarget   ! OK
  class(t3), pointer :: b => mytarget   ! OK
  class(*),  pointer :: c => mytarget   ! OK
  integer,   pointer :: d => mytarget%j ! OK
end program main
