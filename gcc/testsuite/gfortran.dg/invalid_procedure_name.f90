! { dg-do compile }
! PR25061 procedure name conflict
! Test case from PR.
INTERFACE I1 ! { dg-error "" }
 SUBROUTINE S1(I)
 END SUBROUTINE S1
 SUBROUTINE S2(R)
 END SUBROUTINE S2
END INTERFACE I1
CONTAINS
 SUBROUTINE I1(I) ! { dg-error "already defined as a generic" }
 END SUBROUTINE I1
END

