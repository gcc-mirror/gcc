! { dg-do compile }
! PR fortran/89574 - ICE in conv_function_val, at fortran/trans-expr.c:3792

module mod1
contains
  subroutine init
  end subroutine
end module

module mod2
contains
  subroutine init
  end subroutine
end module

module init
  use mod1, only : test_init1 => init
  use mod2, only : test_init2 => init
  implicit none
contains
  subroutine sub
    call test_init1
    call test_init2
    call init
  contains
    subroutine init
    end subroutine
  end subroutine
end module
