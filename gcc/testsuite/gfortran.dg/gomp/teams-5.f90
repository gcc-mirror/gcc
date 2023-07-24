! { dg-do compile }

! PR fortran/110725
! PR middle-end/71065

implicit none
integer :: x
!$omp target device(1)
  block
    !$omp teams num_teams(f())
    !$omp end teams
  end block
!!$omp end target

!$omp target device(1)
  !$omp teams num_teams(f())
  !$omp end teams
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  x = 5
  !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  !$omp end teams
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  !$omp end teams
  x = 5
!$omp end target

!$omp target  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    block
    !$omp teams num_teams(f())
    !$omp end teams
    end block
  end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    x = 5
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
  end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
    x = 5
  end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
    x = 5
  end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
  block; end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    block; end block;
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
  end block
!$omp end target

!$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    !$omp teams num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp end teams
    block; end block;
  end block
!!$omp end target


contains

function f()
  !$omp declare target
  integer, allocatable :: f
  f = 5
end
end

subroutine sub1
  implicit none
  integer :: x,i

  !$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    !$omp teams distribute num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
    x = 7
  end block
  !$omp end target

  !$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    !$omp teams loop num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
    x = 7
  end block
  !$omp end target

  !$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp teams distribute simd num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
    x = 7
  !$omp end target

  !$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    !$omp teams distribute parallel do num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
    x = 7
  !$omp end target

  !$omp target device(1)  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
  block
    x = 7
    !$omp teams distribute parallel do simd num_teams(f())  ! { dg-error "OMP TARGET region at .1. with a nested TEAMS at .2. may not contain any other statement, declaration or directive outside of the single TEAMS construct" }
    do i = 1, 5
    end do
  end block
  !$omp end target

contains

function f()
  !$omp declare target
  integer, allocatable :: f
  f = 5
end

end
