! { dg-do run }
! { dg-options "-O2" }
! { dg-skip-if "NaN not supported" { spu-*-* } { "*" } { "" } }
! Tests that the PRs caused by the lack of gfc_simplify_transfer are
! now fixed. These were brought together in the meta-bug PR31237
! (TRANSFER intrinsic).
! Remaining PRs on 20070409 :-18769 30881 31194 31216 31424 31427
!
program simplify_transfer
  CHARACTER(LEN=100) :: buffer="1.0 3.0"
  call pr18769 ()
  call pr30881 ()
  call pr31194 ()
  call pr31216 ()
  call pr31427 ()
contains
  subroutine pr18769 ()
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
    implicit none
    type t
       integer :: i
    end type t
    type (t), parameter :: u = t (42)
    integer,  parameter :: idx_list(1) = (/ 1 /)
    integer             :: j(1) = transfer (u,  idx_list)
    if (j(1) .ne. 42) call abort ()
  end subroutine pr18769

  subroutine pr30881 ()
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
    INTEGER, PARAMETER :: K=1
    INTEGER ::  I
    I=TRANSFER(.TRUE.,K)
    SELECT CASE(I)
      CASE(TRANSFER(.TRUE.,K))
      CASE(TRANSFER(.FALSE.,K))
        CALL ABORT()
      CASE DEFAULT
        CALL ABORT()
    END SELECT
    I=TRANSFER(.FALSE.,K)
    SELECT CASE(I)
      CASE(TRANSFER(.TRUE.,K))
        CALL ABORT()
      CASE(TRANSFER(.FALSE.,K))
      CASE DEFAULT
      CALL ABORT()
    END SELECT
  END subroutine pr30881

  subroutine pr31194 ()
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
    real(kind(0d0)) :: NaN = transfer(ishft(int(z'FFF80000',8),32),0d0)
    write (buffer,'(e12.5)') NaN
    if (buffer(10:12) .ne. "NaN") call abort ()
  end subroutine pr31194

  subroutine pr31216 ()
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
    INTEGER :: I
    REAL :: C,D
    buffer = "  1.0  3.0"
    READ(buffer,*) C,D
    I=TRANSFER(C/D,I)
    SELECT CASE(I)
      CASE (TRANSFER(1.0/3.0,1))
      CASE DEFAULT
        CALL ABORT()
    END SELECT
  END subroutine pr31216

  subroutine pr31427 ()
!
! Contributed by Michael Richmond <michael.a.richmond@nasa.gov>
!
    INTEGER(KIND=1) :: i(1)
    i = (/ TRANSFER("a", 0_1) /)
    if (i(1) .ne. ichar ("a")) call abort ()
  END subroutine pr31427
end program simplify_transfer
