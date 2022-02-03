! PR fortran/104328
! { dg-do compile }

subroutine foo
  integer :: k = 1
  !$omp atomic compare
  if ( k == 2 ) then	! { dg-error "unexpected !.OMP ATOMIC expression" }
  end if
end
subroutine bar
  real :: x = 1
  !$omp atomic compare
  if ( x == 2 ) then	! { dg-error "unexpected !.OMP ATOMIC expression" }
  end if
end
subroutine baz
  integer :: i
  !$omp atomic capture
  i = 1
  i = i + 1.		! { dg-error "!.OMP ATOMIC capture-statement requires a scalar variable of intrinsic type" }
end
subroutine qux
  integer :: i = 0
  !$omp atomic capture
  i = i + 1.0
  i = i + 1.0		! { dg-error "!.OMP ATOMIC capture-statement requires a scalar variable of intrinsic type" }
end
subroutine garply
  logical :: k = .true.
  !$omp atomic capture compare
  if ( k ) then		! { dg-error "unexpected !.OMP ATOMIC expression" }
  else
  end if
end
