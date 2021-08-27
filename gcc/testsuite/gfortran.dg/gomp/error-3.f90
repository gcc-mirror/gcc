module m
!$omp error asdf			! { dg-error "Failed to match clause" }
!$omp error at				! { dg-error "Expected '\\(' after 'at'" }
!$omp error at(				! { dg-error "Expected COMPILATION or EXECUTION in AT clause at" }
!$omp error at(runtime)			! { dg-error "Expected COMPILATION or EXECUTION in AT clause at" }
!$omp error at(+			! { dg-error "Expected COMPILATION or EXECUTION in AT clause at" }
!$omp error at(compilation		! { dg-error "Expected COMPILATION or EXECUTION in AT clause at" }
!$omp error severity			! { dg-error "Expected '\\(' after 'severity'" }
!$omp error severity(			! { dg-error "Expected FATAL or WARNING in SEVERITY clause at" }
!$omp error severity(error)		! { dg-error "Expected FATAL or WARNING in SEVERITY clause at" }
!$omp error severity(-			! { dg-error "Expected FATAL or WARNING in SEVERITY clause at" }
!$omp error severity(fatal		! { dg-error "Expected FATAL or WARNING in SEVERITY clause at" }
!$omp error message			! { dg-error "Expected '\\(' after 'message'" }
!$omp error message(			! { dg-error "Invalid expression after 'message\\('" }
!$omp error message(0			! { dg-error "Invalid expression after 'message\\('" }
!$omp error message("foo"		! { dg-error "Invalid expression after 'message\\('" }

!$omp error at(compilation) at(compilation)	! { dg-error "Duplicated 'at' clause at" }
!$omp error severity(fatal) severity(warning)	! { dg-error "Duplicated 'severity' clause at" }
!$omp error message("foo") message("foo")	! { dg-error "Duplicated 'message' clause at" }
!$omp error message("foo"),at(compilation),severity(fatal),asdf	! { dg-error "Failed to match clause" }

!$omp error at(execution)			! { dg-error "Unexpected !.OMP ERROR statement in MODULE" }

end module

module m2
character(len=10) :: msg
!$omp error message(1)			! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error message(1.2)		! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error message(4_"foo")		! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error message(["bar","bar"])	! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error message(msg)		! { dg-error "Constant character expression required in MESSAGE clause" }

type S
  !$omp error at(execution) message("foo")! { dg-error "Unexpected !.OMP ERROR statement at" }
  integer s
end type
end module

subroutine bar
character(len=10) :: msg
!$omp error at(execution) message(1)			! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error at(execution) message(1.2)			! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error at(execution) message(4_"foo")		! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error at(execution) message(["bar","bar"])	! { dg-error "MESSAGE clause at .1. requires a scalar default-kind CHARACTER expression" }
!$omp error at(execution) message(msg)			! OK

end

integer function foo (i, x, msg)
  integer :: i
  logical :: x
  character(len=*) :: msg
  !$omp error message(msg)		! { dg-error "Constant character expression required in MESSAGE clause" }
  if (x) then
    !$omp error at(execution)		! OK
  end if
  i = i + 1
  if (x) then
    ;
  else
    !$omp error at(execution) severity(warning)	! OK
  end if
  i = i + 1
  select case (.false.)
    !$omp error severity(fatal) at(execution)	! { dg-error "Expected a CASE or END SELECT statement following SELECT CASE" }
  end select
  do while (.false.)
    !$omp error at(execution)message("42 - 1")	! OK
    i = i + 1
  end do
99  continue
  !$omp error severity(warning) message("bar") at(execution)	! OK
    i = i + 1
  foo = i
end


subroutine foobar
  if (.true.) &  ! { dg-error "Syntax error in IF-clause after" }
    !$omp error at(execution)

  continue

  if (.true.) &  ! { dg-error "Syntax error in IF-clause after" }
    !$omp error  ! { dg-error ".OMP ERROR encountered at" }
end
