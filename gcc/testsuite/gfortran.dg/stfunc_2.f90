! { dg-do compile }
! PR 20467 : we didn't check if a statement function had the dummy attribute.
SUBROUTINE a(b)
        b(c) = 0  ! { dg-error "Unclassifiable statement" }
END SUBROUTINE a

