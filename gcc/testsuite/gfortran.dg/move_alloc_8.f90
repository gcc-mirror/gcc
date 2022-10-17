! { dg-do compile }
!
! PR fortran/50684
!
! Module "bug" contributed by Martin Steghöfer.
!

MODULE BUG
  TYPE MY_TYPE
    INTEGER, ALLOCATABLE :: VALUE
  END TYPE
CONTAINS
  SUBROUTINE POINTER_INTENT_IN_BUG_WORKING(POINTER_INTENT_IN_VARIABLE)
    TYPE(MY_TYPE), POINTER, INTENT(IN) :: POINTER_INTENT_IN_VARIABLE
    TYPE(MY_TYPE), POINTER :: POINTER_VARIABLE_LOCAL
    INTEGER, ALLOCATABLE :: LOCAL_VALUE
    
    POINTER_VARIABLE_LOCAL=>POINTER_INTENT_IN_VARIABLE
    CALL MOVE_ALLOC(POINTER_VARIABLE_LOCAL%VALUE, LOCAL_VALUE)
    
    RETURN
  END SUBROUTINE POINTER_INTENT_IN_BUG_WORKING
  
  SUBROUTINE POINTER_INTENT_IN_BUG_FAILING(POINTER_INTENT_IN_VARIABLE)
    TYPE(MY_TYPE), POINTER, INTENT(IN) :: POINTER_INTENT_IN_VARIABLE
    INTEGER, ALLOCATABLE :: LOCAL_VALUE
    
    CALL MOVE_ALLOC(POINTER_INTENT_IN_VARIABLE%VALUE, LOCAL_VALUE)
    
    RETURN
  END SUBROUTINE POINTER_INTENT_IN_BUG_FAILING
end module bug

subroutine test1()
  TYPE MY_TYPE
    INTEGER, ALLOCATABLE :: VALUE
  END TYPE
CONTAINS
  SUBROUTINE sub (dt)
    type(MY_TYPE), intent(in) :: dt
    INTEGER, ALLOCATABLE :: lv
    call move_alloc(dt%VALUE, lv) ! { dg-error "cannot be INTENT.IN." }
  END SUBROUTINE
end subroutine test1

subroutine test2 (x, px)
  implicit none
  type t
    integer, allocatable :: a
  end type t

  type t2
    type(t), pointer :: ptr
    integer, allocatable :: a
  end type t2

  type(t2), intent(in) :: x
  type(t2), pointer, intent(in) :: px

  integer, allocatable :: a
  type(t2), pointer :: ta

  call move_alloc (px, ta)      ! { dg-error "must be ALLOCATABLE" }
  call move_alloc (x%a, a)      ! { dg-error "cannot be INTENT.IN." }
  call move_alloc (x%ptr%a, a)  ! OK (3)
  call move_alloc (px%a, a)     ! OK (4)
  call move_alloc (px%ptr%a, a) ! OK (5)
end subroutine test2

subroutine test3 (x, px)
  implicit none
  type t
    integer, allocatable :: a
  end type t

  type t2
    class(t), pointer :: ptr
    integer, allocatable :: a
  end type t2

  type(t2), intent(in) :: x
  class(t2), pointer, intent(in) :: px

  integer, allocatable :: a
  class(t2), pointer :: ta

  call move_alloc (px, ta)      ! { dg-error "must be ALLOCATABLE" }
  call move_alloc (x%a, a)      ! { dg-error "cannot be INTENT.IN." }
  call move_alloc (x%ptr%a, a)  ! OK (6)
  call move_alloc (px%a, a)     ! OK (7)
  call move_alloc (px%ptr%a, a) ! OK (8)
end subroutine test3

subroutine test4()
  TYPE MY_TYPE
    INTEGER, ALLOCATABLE :: VALUE
  END TYPE
CONTAINS
  SUBROUTINE sub (dt)
    CLASS(MY_TYPE), intent(in) :: dt
    INTEGER, ALLOCATABLE :: lv
    call move_alloc(dt%VALUE, lv) ! { dg-error "cannot be INTENT.IN." }
  END SUBROUTINE
end subroutine test4
