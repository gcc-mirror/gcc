! { dg-do compile }
! Tests the fix for PR20863 and PR20882, which were concerned with incorrect
! application of constraints associated with "impure" variables in PURE
! procedures.
!
! resolve.c (gfc_impure_variable) detects the following: 
! 12.6 Constraint: In a pure subprogram any variable which is in common or
! accessed by host or use association, is a dummy argument to a pure function,
! is a dummy argument with INTENT (IN) to a pure subroutine, or an object that
! is storage associated with any such variable, shall not be used in the
! following contexts: (clients of this function).  */
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE pr20863
 TYPE node_type
  TYPE(node_type), POINTER :: next=>null()
 END TYPE
CONTAINS
! Original bug - pointer assignments to "impure" derived type with
! pointer component.
  PURE FUNCTION give_next1(node)
     TYPE(node_type), POINTER :: node
     TYPE(node_type), POINTER :: give_next
     give_next => node%next ! { dg-error "Bad target" }
     node%next => give_next ! { dg-error "Bad pointer object" }
  END FUNCTION
! Comment #2
  PURE integer FUNCTION give_next2(i)
     TYPE node_type
       sequence
       TYPE(node_type), POINTER :: next
     END TYPE
     TYPE(node_type), POINTER :: node
     TYPE(node_type), target  :: t
     integer, intent(in)      :: i
     node%next = t          ! This is OK
     give_next2 = i
  END FUNCTION
  PURE FUNCTION give_next3(node)
     TYPE(node_type), intent(in) :: node
     TYPE(node_type) :: give_next
     give_next = node ! { dg-error "impure variable" }
  END FUNCTION
END MODULE pr20863

MODULE pr20882
  TYPE T1
    INTEGER :: I
  END TYPE T1
  TYPE(T1), POINTER :: B
CONTAINS
  PURE FUNCTION TST(A) RESULT(RES)
    TYPE(T1), INTENT(IN), TARGET :: A
    TYPE(T1), POINTER :: RES
    RES => A  ! { dg-error "Bad target" }
    RES => B  ! { dg-error "Bad target" }
    B => RES  ! { dg-error "Bad pointer object" }
  END FUNCTION
  PURE FUNCTION TST2(A) RESULT(RES)
    TYPE(T1), INTENT(IN), TARGET :: A
    TYPE(T1), POINTER :: RES
    allocate (RES)
    RES = A
    B = RES  ! { dg-error "Cannot assign" }
    RES = B
  END FUNCTION
END MODULE pr20882
! { dg-final { cleanup-modules "pr20863 pr20882" } }

