! { dg-do compile }
! Checks for excess errors.
implicit none
dimension i(10) ! { dg-error "has no IMPLICIT type" }
i = 2
end
