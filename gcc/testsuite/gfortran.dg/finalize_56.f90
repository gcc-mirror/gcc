! { dg-do run }
! Test the fix for PR110987
! Segfaulted in runtime, as shown below.
! Contributed by Kirill Chankin  <chilikin.k@gmail.com>
! and John Haiducek  <jhaiduce@gmail.com> (comment 5)
!
MODULE original_mod
  IMPLICIT NONE

  TYPE T1_POINTER
    CLASS(T1), POINTER :: T1
  END TYPE

  TYPE T1
    INTEGER N_NEXT
    CLASS(T1_POINTER), ALLOCATABLE :: NEXT(:)
  CONTAINS
    FINAL :: T1_DESTRUCTOR
    PROCEDURE :: SET_N_NEXT => T1_SET_N_NEXT
    PROCEDURE :: GET_NEXT => T1_GET_NEXT
  END TYPE

  INTERFACE T1
    PROCEDURE T1_CONSTRUCTOR
  END INTERFACE

  TYPE, EXTENDS(T1) :: T2
    REAL X
  CONTAINS
  END TYPE

  INTERFACE T2
    PROCEDURE T2_CONSTRUCTOR
  END INTERFACE

  TYPE, EXTENDS(T1) :: T3
  CONTAINS
    FINAL :: T3_DESTRUCTOR
  END TYPE

  INTERFACE T3
    PROCEDURE T3_CONSTRUCTOR
  END INTERFACE

  INTEGER :: COUNTS = 0

CONTAINS

  TYPE(T1) FUNCTION T1_CONSTRUCTOR() RESULT(L)
    IMPLICIT NONE
    L%N_NEXT = 0
  END FUNCTION

  SUBROUTINE T1_DESTRUCTOR(SELF)
    IMPLICIT NONE
    TYPE(T1), INTENT(INOUT) :: SELF
    IF (ALLOCATED(SELF%NEXT)) THEN
      DEALLOCATE(SELF%NEXT)
    ENDIF
  END SUBROUTINE

  SUBROUTINE T3_DESTRUCTOR(SELF)
    IMPLICIT NONE
    TYPE(T3), INTENT(IN) :: SELF
    if (.NOT.ALLOCATED (SELF%NEXT)) COUNTS = COUNTS + 1
  END SUBROUTINE

  SUBROUTINE T1_SET_N_NEXT(SELF, N_NEXT)
    IMPLICIT NONE
    CLASS(T1), INTENT(INOUT) :: SELF
    INTEGER, INTENT(IN) :: N_NEXT
    INTEGER I
    SELF%N_NEXT = N_NEXT
    ALLOCATE(SELF%NEXT(N_NEXT))
    DO I = 1, N_NEXT
      NULLIFY(SELF%NEXT(I)%T1)
    ENDDO
  END SUBROUTINE

  FUNCTION T1_GET_NEXT(SELF) RESULT(NEXT)
    IMPLICIT NONE
    CLASS(T1), TARGET, INTENT(IN) :: SELF
    CLASS(T1), POINTER :: NEXT
    CLASS(T1), POINTER :: L
    INTEGER I
    IF (SELF%N_NEXT .GE. 1) THEN
      NEXT => SELF%NEXT(1)%T1
      RETURN
    ENDIF
    NULLIFY(NEXT)
  END FUNCTION

  TYPE(T2) FUNCTION T2_CONSTRUCTOR() RESULT(L)
    IMPLICIT NONE
    L%T1 = T1()
    CALL L%T1%SET_N_NEXT(1)
  END FUNCTION

  TYPE(T3) FUNCTION T3_CONSTRUCTOR() RESULT(L)
    IMPLICIT NONE
    L%T1 = T1()
  END FUNCTION

END MODULE original_mod

module comment5_mod
  type::parent
     character(:), allocatable::name
  end type parent
  type, extends(parent)::child
   contains
     final::child_finalize
  end type child
  interface child
     module procedure new_child
  end interface child
  integer :: counts = 0

contains

  type(child) function new_child(name)
    character(*)::name
    new_child%name=name
  end function new_child

  subroutine child_finalize(this)
    type(child), intent(in)::this
    counts = counts + 1
  end subroutine child_finalize
end module comment5_mod

PROGRAM TEST_PROGRAM
  call original
  call comment5
contains
  subroutine original
    USE original_mod
    IMPLICIT NONE
    TYPE(T1), TARGET :: X1
    TYPE(T2), TARGET :: X2
    TYPE(T3), TARGET :: X3
    CLASS(T1), POINTER :: L
    X1 = T1()
    X2 = T2()
    X2%NEXT(1)%T1 => X1
    X3 = T3()
    CALL X3%SET_N_NEXT(1)
    X3%NEXT(1)%T1 => X2
    L => X3
    DO WHILE (.TRUE.)
      L => L%GET_NEXT()                 ! Used to segfault here in runtime
      IF (.NOT. ASSOCIATED(L)) EXIT
      COUNTS = COUNTS + 1
    ENDDO
! Two for T3 finalization and two for associated 'L's
    IF (COUNTS .NE. 4) STOP 1
  end subroutine original

  subroutine comment5
    use comment5_mod, only: child, counts
    implicit none
    type(child)::kid
    kid = child("Name")
    if (.not.allocated (kid%name)) stop 2
    if (kid%name .ne. "Name") stop 3
    if (counts .ne. 2) stop 4
  end subroutine comment5
END PROGRAM
