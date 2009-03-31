! { dg-do compile }
! Test the fixes for PR38917 and 38918, in which the NULL values caused errors.
!
! Contributed by Dick Hendrickson  <dick.hendrickson@gmail.com>
!             and Tobias Burnus  <burnus@gcc.gnu.org>
!
      SUBROUTINE PF0009
!  PR38918
      TYPE  :: HAS_POINTER
        INTEGER, POINTER            :: PTR_S
      END TYPE HAS_POINTER
      TYPE (HAS_POINTER)  ::  PTR_ARRAY(5)

      DATA PTR_ARRAY(1)%PTR_S  /NULL()/

      end subroutine pf0009

      SUBROUTINE PF0005
! PR38917
      REAL, SAVE, POINTER :: PTR1
      INTEGER, POINTER       :: PTR2(:,:,:)
      CHARACTER(LEN=1), SAVE, POINTER :: PTR3(:)

      DATA  PTR1 / NULL() /
      DATA  PTR2 / NULL() /
      DATA  PTR3 / NULL() /

      end subroutine pf0005

! Tobias pointed out that this would cause an ICE rather than an error.
      subroutine tobias
      integer, pointer :: ptr(:)
      data ptr(1) /NULL()/  ! { dg-error "must be a full array" }
      end subroutine tobias

