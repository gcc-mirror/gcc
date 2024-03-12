! { dg-do compile }

! PR fortran/110725
! PR middle-end/71065


subroutine one
!$omp target  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
block
  integer :: i   ! <<< invalid: variable declaration
  !$omp teams  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  i = 5
  !$omp end teams
end block

!$omp target  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
block
  type t   ! <<< invalid: type declaration
  end type t
  !$omp teams  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  i = 5
  !$omp end teams
end block

!$omp target
  ! The following is invalid - but not detected as ST_NONE is returned:
  !$omp error at(compilation) severity(warning)  ! { dg-warning "OMP ERROR encountered" }
  !$omp teams
  i = 5
  !$omp end teams
!$omp end target

!$omp target
  ! The following is invalid - but not detected as ST_NONE is returned:
  !$omp nothing ! <<< invalid: directive
  !$omp teams
  i = 5
  !$omp end teams
!$omp end target


!$omp target  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
block
  do i = 5, 8
    !$omp teams
    block; end block
  end do
end block

end


subroutine two
!$omp target ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
block
  integer :: i   ! <<< invalid: variable declaration
  !$omp teams distribute  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
  !$omp end teams distribute
end block

!$omp target  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
block
  type t   ! <<< invalid: type declaration
  end type t
  !$omp teams distribute parallel do ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  do i = 1, 5
  end do
end block

!$omp target
  ! The following is invalid - but not detected as ST_NONE is returned:
  !$omp error at(compilation) severity(warning)  ! { dg-warning "OMP ERROR encountered" }
  !$omp teams loop
  do i = 5, 10
  end do
!$omp end target

!$omp target
  ! The following is invalid - but not detected as ST_NONE is returned:
  !$omp nothing ! <<< invalid: directive
  !$omp teams distribute simd
  do i = -3, 5
  end do
  !$omp end teams distribute simd
!$omp end target
end
