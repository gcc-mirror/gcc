! { dg-do compile }
!
! Error checking for the SELECT TYPE statement
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

  type :: t1
    integer :: i = 42
    class(t1),pointer :: cp
  end type

  type, extends(t1) :: t2
    integer :: j = 99
  end type

  type :: t3
    real :: r
  end type

  type :: ts
    sequence
    integer :: k = 5
  end type

  class(t1), pointer :: a => NULL()
  type(t1), target :: b
  type(t2), target :: c
  a => b
  print *, a%i

  type is (t1)  ! { dg-error "Unexpected TYPE IS statement" }

  select type (3.5)  ! { dg-error "is not a named variable" }
  select type (a%cp) ! { dg-error "is not a named variable" }
  select type (b)    ! { dg-error "Selector shall be polymorphic" }
  end select

  select type (a)
    print *,"hello world!"  ! { dg-error "Expected TYPE IS, CLASS IS or END SELECT" }
  type is (t1)
    print *,"a is TYPE(t1)"
  type is (t2)
    print *,"a is TYPE(t2)"
  class is (ts)  ! { dg-error "must be extensible" }
    print *,"a is TYPE(ts)"
  type is (t3)   ! { dg-error "must be an extension of" }
    print *,"a is TYPE(t3)"
  type is (t4)   ! { dg-error "error in TYPE IS specification" }
    print *,"a is TYPE(t3)"
  class is (t1)
    print *,"a is CLASS(t1)"
  class is (t2) label  ! { dg-error "Syntax error" }
    print *,"a is CLASS(t2)"
  class default  ! { dg-error "cannot be followed by a second DEFAULT CASE" }
    print *,"default"
  class default  ! { dg-error "cannot be followed by a second DEFAULT CASE" }
    print *,"default2"
  end select

label: select type (a)
  type is (t1) label
    print *,"a is TYPE(t1)"
  type is (t2)  ! { dg-error "overlaps with CASE label" }
    print *,"a is TYPE(t2)"
  type is (t2)  ! { dg-error "overlaps with CASE label" }
    print *,"a is still TYPE(t2)"
  class is (t1) labe   ! { dg-error "Expected block name" }
    print *,"a is CLASS(t1)"
  end select label

end
