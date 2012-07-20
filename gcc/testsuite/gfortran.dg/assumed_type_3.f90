! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/48820
!
! Test TYPE(*)

subroutine one(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
  type(*), value :: a
end subroutine one

subroutine two(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
  type(*), pointer :: a
end subroutine two

subroutine three(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
  type(*), allocatable :: a
end subroutine three

subroutine four(a) ! { dg-error "may not have the ALLOCATABLE, CODIMENSION, POINTER or VALUE attribute" }
  type(*)  :: a[*]
end subroutine four

subroutine five(a) ! { dg-error "shall not be an explicit-shape array" }
  type(*) :: a(3)
end subroutine five

subroutine six()
  type(*) :: nodum ! { dg-error "is only permitted for dummy variables" }
end subroutine six

subroutine seven(y)
 type(*) :: y(:)
 call a7(y(3:5)) ! { dg-error "Assumed-type variable y at .1. shall not have a subobject reference" }
contains
 subroutine a7(x)
   type(*) :: x(*)
 end subroutine a7
end subroutine seven

subroutine eight()
  type t
    type(*) :: x ! { dg-error "is not allowed for components" }
  end type t
end subroutine eight

subroutine nine()
  interface one
    subroutine okay(x)
      type(*) :: x
    end subroutine okay
    subroutine okay2(x)
      type(*) :: x(*)
    end subroutine okay2
    subroutine okay3(x,y)
      integer :: x
      type(*) :: y
    end subroutine okay3
  end interface
  interface two
    subroutine okok1(x)
      type(*) :: x
    end subroutine okok1
    subroutine okok2(x)
      integer :: x(*)
    end subroutine okok2
  end interface
  interface three
    subroutine ambig1(x)
      type(*) :: x
    end subroutine ambig1
    subroutine ambig2(x)
      integer :: x
    end subroutine ambig2 ! { dg-error "Ambiguous interfaces 'ambig2' and 'ambig1' in generic interface 'three'" }
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
    type(*) :: a
  end subroutine sub
end subroutine ten

subroutine eleven(x)
  external bar
  type(*) :: x
  call bar(x) ! { dg-error "Assumed-type argument x at .1. requires an explicit interface" }
end subroutine eleven

subroutine twelf(x)
  type(*) :: x
  call bar(x) ! { dg-error "Type mismatch in argument" }
contains
  subroutine bar(x)
    integer :: x
  end subroutine bar
end subroutine twelf

subroutine thirteen(x, y)
  type(*) :: x
  integer :: y(:)
  print *, ubound(y, dim=x) ! { dg-error "must be INTEGER" }
end subroutine thirteen

subroutine fourteen(x)
  type(*) :: x
  x = x ! { dg-error "Assumed-type variable x at .1. may only be used as actual argument" }
end subroutine fourteen
