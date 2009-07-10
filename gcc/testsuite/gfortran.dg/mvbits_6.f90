! { dg-do compile }

! PR fortran/38883
! This ICE'd because the temporary-creation in the MVBITS call was wrong.
! This is the original test from the PR, the complicated version.

! Contributed by Dick Hendrickson <dick.hendrickson@gmail.com>

     module yg0009_stuff

      type unseq
         integer I
      end type

      contains

      SUBROUTINE YG0009(TDA2L,NF4,NF3,NF1,MF1,MF4,MF3)
        TYPE(UNSEQ) TDA2L(NF4,NF3)

        CALL MVBITS (TDA2L(NF4:NF1:MF1,NF1:NF3)%I,2, &
          4, TDA2L(-MF4:-MF1:-NF1,-MF1:-MF3)%I, 3)

      END SUBROUTINE

      end module yg0009_stuff

      program try_yg0009
      use yg0009_stuff
      type(unseq)  tda2l(4,3)

      call yg0009(tda2l,4,3,1,-1,-4,-3)

      end
! { dg-final { cleanup-modules "yg0009_stuff" } }
