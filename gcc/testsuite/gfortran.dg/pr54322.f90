! { dg-do compile }
!
! PR 54322
! An associate name has neither the POINTER nor the ALLOCATABLE attribute,
! and has the TARGET attribute only if the selector is a variable with the
! TARGET or POINTER attribute (F2023 11.1.3.3).

module m
  implicit none
  type :: t
    real :: x
  end type
  type, extends(t) :: t3
    real :: z
  end type
end module

program p
  use m
  implicit none
  class(t), allocatable :: ca
  class(t), pointer :: cp
  class(t3), pointer :: p3
  integer, pointer :: q
  integer, target :: i
  type(t), target :: tt

  allocate (t3 :: ca)
  q => i
  cp => tt

  select type (an => ca)
  type is (t3)
    p3 => an                ! { dg-error "neither TARGET nor POINTER" }
  class is (t)
    print *, allocated (an) ! { dg-error "must be ALLOCATABLE" }
  end select

  associate (a => q)
    a => i                  ! { dg-error "Non-POINTER in pointer association" }
    nullify (a)             ! { dg-error "Non-POINTER in pointer association" }
    print *, associated (a) ! { dg-error "must be a POINTER" }
  end associate

  associate (a => cp)
    a => tt                 ! { dg-error "Non-POINTER in pointer association" }
  end associate

  select type (an => cp)
  type is (t)
    an => tt                ! { dg-error "Non-POINTER in pointer association" }
  end select

  ! A pointer selector makes the associate name a target.
  select type (an => cp)
  type is (t3)
    p3 => an
  end select
  associate (a => q)
    q => a
  end associate
end program
