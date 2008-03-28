! { dg-do run }
!
! PR fortran/35721
!
! ASSOCIATED(ptr, trgt) should return true if
! the same storage units (in the same order)
! gfortran was returning false if the strips
! were different but only one (the same!) element
! was present.
!
! Contributed by Dick Hendrickson
!
      program try_mg0028
      implicit none
      real  tda2r(2,3)

      call       mg0028(tda2r,  1,  2,  3)

      CONTAINS

      SUBROUTINE MG0028(TDA2R,nf1,nf2,nf3)
      integer        ::  nf1,nf2,nf3
      real, target   ::  TDA2R(NF2,NF3)
      real, pointer  ::  TLA2L(:,:),TLA2L1(:,:)
      logical LL(4)
      TLA2L => TDA2R(NF2:NF1:-NF2,NF3:NF1:-NF2)
      TLA2L1 => TLA2L
      LL(1) = ASSOCIATED(TLA2L)
      LL(2) = ASSOCIATED(TLA2L,TLA2L1)
      LL(3) = ASSOCIATED(TLA2L,TDA2R)
      LL(4) = ASSOCIATED(TLA2L1,TDA2R(2:2,3:1:-2))  !should be true

      if (any(LL .neqv. (/ .true., .true., .false., .true./))) then
        print *, LL
        print *, shape(TLA2L1)
        print *, shape(TDA2R(2:2,3:1:-2))
        stop
      endif

      END SUBROUTINE
      END PROGRAM
