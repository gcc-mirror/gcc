module m
  integer :: v
  interface
    subroutine foo(i)
      integer :: i
    end
  end interface
end

subroutine bar
  use m
  implicit none
  integer :: i
  !$omp taskloop reduction (task, +: v)	! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  do i = 0, 63
    call foo (i)
  end do
  !$omp taskloop simd reduction (task, +: v)	! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  do i = 0, 63
    v = v + 1
  end do
  !$omp teams reduction (task, +: v)	! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  call foo (i)
  !$omp end teams
  !$omp teams distribute reduction (task, +: v)	! { dg-error "Only DEFAULT permitted as reduction-modifier in REDUCTION clause" }
  do i = 0, 63
    call foo (i)
  end do
  !$omp end teams distribute
end
