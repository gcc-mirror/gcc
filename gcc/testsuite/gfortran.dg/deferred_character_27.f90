! { dg-do compile }
!
! Make sure that PR82617 remains fixed. The first attempt at a
! fix for PR70752 cause this to ICE at the point indicated below.
!
! Contributed by Ogmundur Petersson  <uberprugelknabe@hotmail.com>
!
MODULE test

  IMPLICIT NONE

  PRIVATE
  PUBLIC str_words

  !> Characters that are considered whitespace.
  CHARACTER(len=*), PARAMETER :: strwhitespace = &
    char(32)//& ! space
    char(10)//& ! new line
    char(13)//& ! carriage return
    char( 9)//& ! horizontal tab
    char(11)//& ! vertical tab
    char(12)    ! form feed (new page)

  CONTAINS

  ! -------------------------------------------------------------------
  !> Split string into words separated by arbitrary strings of whitespace
  !> characters (space, tab, newline, return, formfeed).
  FUNCTION str_words(str,white) RESULT(items)
    CHARACTER(len=:), DIMENSION(:), ALLOCATABLE :: items
    CHARACTER(len=*), INTENT(in) :: str !< String to split.
    CHARACTER(len=*), INTENT(in) :: white ! Whitespace characters.

    items = strwords_impl(str,white)

  END FUNCTION str_words

  ! -------------------------------------------------------------------
  !>Implementation of str_words
  !> characters (space, tab, newline, return, formfeed).
  FUNCTION strwords_impl(str,white) RESULT(items)
    CHARACTER(len=:), DIMENSION(:), ALLOCATABLE :: items
    CHARACTER(len=*), INTENT(in) :: str !< String to split.
    CHARACTER(len=*), INTENT(in) :: white ! Whitespace characters.

    INTEGER :: i0,i1,n
    INTEGER :: l_item,i_item,n_item

    n = verify(str,white,.TRUE.)
    IF (n>0) THEN
      n_item = 0
      l_item = 0
      i1 = 0
      DO
        i0 = verify(str(i1+1:n),white)+i1
        i1 = scan(str(i0+1:n),white)
        n_item = n_item+1
        IF (i1>0) THEN
          l_item = max(l_item,i1)
          i1 = i0+i1
        ELSE
          l_item = max(l_item,n-i0+1)
          EXIT
        END IF
      END DO
      ALLOCATE(CHARACTER(len=l_item)::items(n_item))
      i_item = 0
      i1 = 0
      DO
        i0 = verify(str(i1+1:n),white)+i1
        i1 = scan(str(i0+1:n),white)
        i_item = i_item+1
        IF (i1>0) THEN
          i1 = i0+i1
          items(i_item) = str(i0:i1-1)
        ELSE
          items(i_item) = str(i0:n)
          EXIT
        END IF
      END DO
    ELSE
      ALLOCATE(CHARACTER(len=0)::items(0))
    END IF

  END FUNCTION strwords_impl

END MODULE test
