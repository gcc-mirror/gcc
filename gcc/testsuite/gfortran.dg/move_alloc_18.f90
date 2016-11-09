! { dg-do compile }
!
! Test that the anti-aliasing restriction does not knock out valid code.
!
! Contributed by  Andrew Balwin on
! https://groups.google.com/forum/#!topic/comp.lang.fortran/oiXdl1LPb_s
!
      PROGRAM TEST
        IMPLICIT NONE

        TYPE FOOBAR
          INTEGER, ALLOCATABLE :: COMP(:)
        END TYPE

        TYPE (FOOBAR) :: MY_ARRAY(6)

        ALLOCATE (MY_ARRAY(1)%COMP(10))

        CALL MOVE_ALLOC (MY_ARRAY(1)%COMP, MY_ARRAY(2)%COMP)

      END PROGRAM TEST
