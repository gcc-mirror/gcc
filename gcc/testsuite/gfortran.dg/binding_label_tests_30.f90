! { dg-do compile }
! Make sure this error is flagged.
subroutine foo() ! { dg-error "is already being used as a SUBROUTINE" }
end subroutine foo

subroutine bar() bind(C,name="foo") ! { dg-error "is already being used as a SUBROUTINE" }
end subroutine bar
