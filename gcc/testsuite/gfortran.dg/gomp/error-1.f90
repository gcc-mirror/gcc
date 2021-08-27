! { dg-additional-options "-ffree-line-length-none" }
module m
!$omp error			! { dg-error ".OMP ERROR encountered at .1." }
!$omp error at(compilation)	! { dg-error ".OMP ERROR encountered at .1." }
!$omp error severity(fatal)	! { dg-error ".OMP ERROR encountered at .1." }
!$omp error message("my msg")	! { dg-error ".OMP ERROR encountered at .1.: my msg" }
!$omp error severity(warning)message("another message")at(compilation)	! { dg-warning ".OMP ERROR encountered at .1.: another message" }

type S
  !$omp error			! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error at(compilation)	! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error severity(fatal)	! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error message("42")	! { dg-error ".OMP ERROR encountered at .1.: 42" }
  !$omp error severity(warning), message("foo"), at(compilation)	! { dg-warning ".OMP ERROR encountered at .1.: foo" }
  integer s
end type S
end module m

integer function foo (i, x)
  integer :: i
  logical :: x
  !$omp error			! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error at(compilation)	! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error severity(fatal)	! { dg-error ".OMP ERROR encountered at .1." }
  !$omp error message("42 / 1")	! { dg-error ".OMP ERROR encountered at .1.: 42 / 1" }
  !$omp error severity(warning) message("bar") at(compilation)	! { dg-warning ".OMP ERROR encountered at .1.: bar" }
  if (x) then
    !$omp error			! { dg-error ".OMP ERROR encountered at .1." }
    i = i + 1
  end if
  if (x) then
    ;
  else
    !$omp error at(compilation)	! { dg-error ".OMP ERROR encountered at .1." }
    i = i + 1
  end if
  select case (.false.)
    !$omp error severity(fatal)	! { dg-error ".OMP ERROR encountered at .1." }
    case default
      !
  end select
  do while (.false.)
    !$omp error message("42 - 1")	! { dg-error ".OMP ERROR encountered at .1.: 42 - 1" }
    i = i + 1
  end do
  lab:
  !$omp error severity(warning) message("bar") at(compilation)	! { dg-warning ".OMP ERROR encountered at .1.: bar" }
    i++;
  foo = i
  return
end
