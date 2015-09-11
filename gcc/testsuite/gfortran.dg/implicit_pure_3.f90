! { dg-do compile }
! { dg-options "-O2 -fdump-tree-optimized" }
!
! PR fortran/54556
!
! Contributed by Joost VandeVondele
!
MODULE parallel_rng_types

  IMPLICIT NONE

  ! Global parameters in this module
  INTEGER, PARAMETER :: dp=8

  TYPE rng_stream_type
    PRIVATE
    CHARACTER(LEN=40)             :: name
    INTEGER                       :: distribution_type
    REAL(KIND=dp), DIMENSION(3,2) :: bg,cg,ig
    LOGICAL                       :: antithetic,extended_precision
    REAL(KIND=dp)                 :: buffer
    LOGICAL                       :: buffer_filled
  END TYPE rng_stream_type

  REAL(KIND=dp), DIMENSION(3,3) :: a1p0,a1p76,a1p127,&
                                   a2p0,a2p76,a2p127,&
                                   inv_a1,inv_a2

  INTEGER, PARAMETER          :: GAUSSIAN = 1,&
                                 UNIFORM  = 2

  REAL(KIND=dp), PARAMETER :: norm  = 2.328306549295727688e-10_dp,&
                              m1    = 4294967087.0_dp,&
                              m2    = 4294944443.0_dp,&
                              a12   = 1403580.0_dp,&
                              a13n  = 810728.0_dp,&
                              a21   = 527612.0_dp,&
                              a23n  = 1370589.0_dp,&
                              two17 = 131072.0_dp,&            ! 2**17
                              two53 = 9007199254740992.0_dp,&  ! 2**53
                              fact  = 5.9604644775390625e-8_dp ! 1/2**24


CONTAINS

  FUNCTION rn32(rng_stream) RESULT(u)

    TYPE(rng_stream_type), POINTER           :: rng_stream
    REAL(KIND=dp)                            :: u

    INTEGER                                  :: k
    REAL(KIND=dp)                            :: p1, p2

! -------------------------------------------------------------------------
! Component 1

    p1 = a12*rng_stream%cg(2,1) - a13n*rng_stream%cg(1,1)
    k = INT(p1/m1)
    p1 = p1 - k*m1
    IF (p1 < 0.0_dp) p1 = p1 + m1
    rng_stream%cg(1,1) = rng_stream%cg(2,1)
    rng_stream%cg(2,1) = rng_stream%cg(3,1)
    rng_stream%cg(3,1) = p1

    ! Component 2

    p2 = a21*rng_stream%cg(3,2) - a23n*rng_stream%cg(1,2)
    k = INT(p2/m2)
    p2 = p2 - k*m2
    IF (p2 < 0.0_dp) p2 = p2 + m2
    rng_stream%cg(1,2) = rng_stream%cg(2,2)
    rng_stream%cg(2,2) = rng_stream%cg(3,2)
    rng_stream%cg(3,2) = p2

    ! Combination

    IF (p1 > p2) THEN
      u = (p1 - p2)*norm
    ELSE
      u = (p1 - p2 + m1)*norm
    END IF

    IF (rng_stream%antithetic) u = 1.0_dp - u

  END FUNCTION rn32

! *****************************************************************************
  FUNCTION rn53(rng_stream) RESULT(u)

    TYPE(rng_stream_type), POINTER           :: rng_stream
    REAL(KIND=dp)                            :: u

    u = rn32(rng_stream)

    IF (rng_stream%antithetic) THEN
      u = u + (rn32(rng_stream) - 1.0_dp)*fact
      IF (u < 0.0_dp) u = u + 1.0_dp
    ELSE
      u = u + rn32(rng_stream)*fact
      IF (u >= 1.0_dp) u = u - 1.0_dp
    END IF

  END FUNCTION rn53

END MODULE

! { dg-final { scan-module-absence "parallel_rng_types" "IMPLICIT_PURE" } }
! { dg-final { scan-tree-dump-times "rn32 \\(rng_stream" 3 "optimized" } }
