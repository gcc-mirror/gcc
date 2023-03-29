! { dg-do compile }
! { dg-additional-options "-fdump-tree-original" }
!
! PR fortran/105012
! The following case was triggering an ICE because of a clobber
! on the DERFC function decl instead of its result.

module error_function
integer, parameter :: r8 = selected_real_kind(12) ! 8 byte real
contains
SUBROUTINE CALERF_r8(ARG, RESULT, JINT)
   integer, parameter :: rk = r8
   real(rk), intent(in)  :: arg
   real(rk), intent(out) :: result
   IF (Y .LE. THRESH) THEN
   END IF
end SUBROUTINE CALERF_r8
FUNCTION DERFC(X)
   integer, parameter :: rk = r8 ! 8 byte real
   real(rk), intent(in) :: X
   real(rk) :: DERFC
   CALL CALERF_r8(X, DERFC, JINT)
END FUNCTION DERFC
end module error_function

! { dg-final { scan-tree-dump-times "CLOBBER" 1 "original" } }
! { dg-final { scan-tree-dump "__result_derfc = {CLOBBER};" "original" } }
