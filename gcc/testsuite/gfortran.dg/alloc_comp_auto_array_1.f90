! { dg-do run }
! Fix for PR29699 - see below for details.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
PROGRAM vocabulary_word_count

  IMPLICIT NONE
  TYPE VARYING_STRING
    CHARACTER,DIMENSION(:),ALLOCATABLE :: chars
  ENDTYPE VARYING_STRING

  INTEGER :: list_size=200

  call extend_lists2

CONTAINS

! First the original problem: vocab_swap not being referenced caused
! an ICE because default initialization is used, which results in a
! call to gfc_conv_variable, which calls gfc_get_symbol_decl.

  SUBROUTINE extend_lists1
    type(VARYING_STRING),DIMENSION(list_size) :: vocab_swap
  ENDSUBROUTINE extend_lists1

! Curing this then uncovered two more problems: If vocab_swap were
! actually referenced, an ICE occurred in the gimplifier because
! the declaration for this automatic array is presented as a
! pointer to the array, rather than the array. Curing this allows
! the code to compile but it bombed out at run time because the
! malloc/free occurred in the wrong order with respect to the
! nullify/deallocate of the allocatable components.

  SUBROUTINE extend_lists2
    type(VARYING_STRING),DIMENSION(list_size) :: vocab_swap
    allocate (vocab_swap(1)%chars(10))
    if (.not.allocated(vocab_swap(1)%chars)) call abort ()
    if (allocated(vocab_swap(10)%chars)) call abort ()
  ENDSUBROUTINE extend_lists2
  
ENDPROGRAM vocabulary_word_count
