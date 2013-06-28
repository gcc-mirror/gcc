! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/39505
! 
! Test NO_ARG_CHECK
! Copied from assumed_type_2.f90
!
subroutine one(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
!GCC$ attributes NO_ARG_CHECK :: a
  integer, value :: a
end subroutine one

subroutine two(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
!GCC$ attributes NO_ARG_CHECK :: a
  integer, pointer :: a
end subroutine two

subroutine three(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
!GCC$ attributes NO_ARG_CHECK :: a
  integer, allocatable :: a
end subroutine three

subroutine four(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
!GCC$ attributes NO_ARG_CHECK :: a
  integer  :: a[*]
end subroutine four

subroutine five(a) ! { dg-error "with NO_ARG_CHECK attribute shall either be a scalar or an assumed-size array" }
!GCC$ attributes NO_ARG_CHECK :: a
  integer :: a(3)
end subroutine five

subroutine six()
!GCC$ attributes NO_ARG_CHECK :: nodum ! { dg-error "with NO_ARG_CHECK attribute shall be a dummy argument" }
  integer :: nodum
end subroutine six

subroutine seven(y)
!GCC$ attributes NO_ARG_CHECK :: y
 integer :: y(*)
 call a7(y(3:5)) ! { dg-error "with NO_ARG_CHECK attribute shall not have a subobject reference" }
contains
 subroutine a7(x)
!GCC$ attributes NO_ARG_CHECK :: x
   integer :: x(*)
 end subroutine a7
end subroutine seven

subroutine nine()
  interface one
    subroutine okay(x)
!GCC$ attributes NO_ARG_CHECK :: x
      integer :: x
    end subroutine okay
  end interface
  interface two
    subroutine ambig1(x)
!GCC$ attributes NO_ARG_CHECK :: x
      integer :: x
    end subroutine ambig1
    subroutine ambig2(x)
!GCC$ attributes NO_ARG_CHECK :: x
      integer :: x(*)
    end subroutine ambig2 ! { dg-error "Ambiguous interfaces 'ambig2' and 'ambig1' in generic interface 'two'" }
  end interface
  interface three
    subroutine ambig3(x)
!GCC$ attributes NO_ARG_CHECK :: x
      integer :: x
    end subroutine ambig3
    subroutine ambig4(x)
      integer :: x
    end subroutine ambig4 ! { dg-error "Ambiguous interfaces 'ambig4' and 'ambig3' in generic interface 'three'" }
  end interface
end subroutine nine

subroutine ten()
 interface
   subroutine bar()
   end subroutine
 end interface
 type t
 contains
   procedure, nopass :: proc => bar
 end type
 type(t) :: xx
 call sub(xx) ! { dg-error "is of derived type with type-bound or FINAL procedures" }
contains
  subroutine sub(a)
!GCC$ attributes NO_ARG_CHECK :: a
    integer :: a
  end subroutine sub
end subroutine ten

subroutine eleven(x)
  external bar
!GCC$ attributes NO_ARG_CHECK :: x
  integer :: x
  call bar(x) ! { dg-error "Assumed-type argument x at .1. requires an explicit interface" }
end subroutine eleven

subroutine twelf(x)
!GCC$ attributes NO_ARG_CHECK :: x
  integer :: x
  call bar(x) ! { dg-error "Type mismatch in argument" }
contains
  subroutine bar(x)
    integer :: x
  end subroutine bar
end subroutine twelf

subroutine thirteen(x, y)
!GCC$ attributes NO_ARG_CHECK :: x
  integer :: x
  integer :: y(:)
  print *, ubound(y, dim=x) ! { dg-error "Variable with NO_ARG_CHECK attribute at .1. is only permitted as argument to the intrinsic functions C_LOC and PRESENT" }
end subroutine thirteen

subroutine fourteen(x)
!GCC$ attributes NO_ARG_CHECK :: x
  integer :: x
  x = x ! { dg-error "with NO_ARG_CHECK attribute may only be used as actual argument" }
end subroutine fourteen
