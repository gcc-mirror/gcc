module m
  integer a, b
end module m

subroutine f1
  use m
  !$omp scan inclusive (a)  ! { dg-error "Unexpected ..OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }
  !$omp scan exclusive (b)  ! { dg-error "Unexpected ..OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }
end

subroutine f2 (c, d, e, f)
  use m
  implicit none
  integer i, l, c(*), d(*), e(64), f(64)
  l = 1

  !$omp do reduction (inscan, +: a) reduction (+: b)  ! { dg-error "'inscan' and non-'inscan' 'reduction' clauses on the same construct" }
  do i = 1, 64
    block
      b = b + 1
      a = a + c(i)
    end block
    !$omp scan inclusive (a)
    d(i) = a
  end do

  !$omp do reduction (+: a) reduction (inscan, +: b)  ! { dg-error "'inscan' and non-'inscan' 'reduction' clauses on the same construct" }
  do i = 1, 64
    block
      a = a + 1
      b = b + c(i)
    end block
    !$omp scan inclusive (b)
      d(i) = b
  end do

  !$omp do reduction (inscan, +: e)
  do i = 1, 64
    block
      e(1) = e(1) + c(i)
      e(2) = e(2) + c(i)
    end block
    !$omp scan inclusive (a, e)
    block
      d(1) = e(1)
      f(2) = e(2)
    end block
  end do

  !$omp do reduction (inscan, +: e(:2))  ! { dg-error "Syntax error in OpenMP variable list" }
  do i = 1, 64
    block
      e(1) = e(1) + c(i)
      e(2) = e(2) + c(i)
    end block
    !$omp scan inclusive (a, e) ! { dg-error "outside loop construct with 'inscan' REDUCTION clause" }
    block
      d(1) = e(1)
      f(2) = e(2)
    end block
  end do

  !$omp do reduction (inscan, +: a) ordered    ! { dg-error "ORDERED clause specified together with 'inscan' REDUCTION clause" }
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a)
    d(i) = a
  end do

  !$omp do reduction (inscan, +: a) ordered(1)    ! { dg-error "ORDERED clause specified together with 'inscan' REDUCTION clause" }
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a)
    d(i) = a
  end do

  !$omp do reduction (inscan, +: a) schedule(static)  ! { dg-error "SCHEDULE clause specified together with 'inscan' REDUCTION clause" }
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a)
    d(i) = a
  end do

  !$omp do reduction (inscan, +: a) schedule(static, 2)  ! { dg-error "SCHEDULE clause specified together with 'inscan' REDUCTION clause" }
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a)
    d(i) = a
  end do

  !$omp do reduction (inscan, +: a) schedule(nonmonotonic: dynamic, 2)  ! { dg-error "SCHEDULE clause specified together with 'inscan' REDUCTION clause" }
  do i = 1, 64
    a = a + c(i)
    !$omp scan inclusive (a)
    d(i) = a
  end do
end

subroutine f3 (c, d)
  use m
  implicit none
  integer i, c(64), d(64)
  !$omp teams reduction (inscan, +: a)
    ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
    ! ...
  !$omp end teams

  !$omp scope reduction (inscan, +: a)
    ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
    ! ...
  !$omp end scope

  !$omp target parallel do reduction (inscan, +: a) map (c, d)
  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a)
    a = a + c(i)
  end do
  !$omp teams
  !$omp distribute parallel do reduction (inscan, +: a)
  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a)
    a = a + c(i)
  end do
  !$omp end teams

  !$omp distribute parallel do simd reduction (inscan, +: a)
  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a)
    a = a + c(i)
  end do
end

subroutine f4 (c, d)
  use m
  implicit none
  integer i, c(64), d(64)
  !$omp taskloop reduction (inscan, +: a)
  ! { dg-error "'inscan' REDUCTION clause on construct other than DO, SIMD, DO SIMD, PARALLEL DO, PARALLEL DO SIMD" "" { target *-*-* } .-1 }
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a)
    a = a + c(i)
  end do
end

subroutine f7
  use m
  implicit none
  integer i
  !$omp simd reduction (inscan, +: a)
  do i = 1, 64
    if (i == 23) then  ! { dg-error "invalid exit from OpenMP structured block" "" { target c++ } .+1 }
      cycle  ! { dg-error "invalid branch to/from OpenMP structured block" "" { target c } }
    elseif (i == 27) then
      goto 123  ! Diagnostic by ME, see scan-7.f90
      ! { dg-warning "is not in the same block as the GOTO statement" "" { target *-*-* } .-1 }
    endif
    !$omp scan exclusive (a)
    block
123 a = 0  ! { dg-error "jump to label 'l1'" "" { target c++ } }
           ! { dg-warning "is not in the same block as the GOTO statement" "" { target *-*-* } .-1 }
      if (i == 33) then  ! { dg-error "invalid exit from OpenMP structured block" "" { target c++ } .+1 }
        cycle  ! { dg-error "invalid branch to/from OpenMP structured block" "" { target c } }
      end if
    end block
  end do
end

subroutine f8 (c, d, e, f)
  use m
  implicit none
  integer i, c(64), d(64), e(64), f(64)
  !$omp do reduction (inscan, +: a, b)	  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured-block-sequences" }
  do i = 1, 64
    block
      a = a + c(i)
      b = b + d(i)
    end block
    !$omp scan inclusive (a) inclusive (b)  ! { dg-error "Unexpected junk after ..OMP SCAN" }
    block
      e(i) = a
      f(i) = b
    end block
  end do

  !$omp do reduction (inscan, +: a, b)  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured-block-sequences" }
  do i = 1, 64
    block
      a = a + c(i)
      b = b + d(i)
    end block
    !$omp scan  ! { dg-error "Expected INCLUSIVE or EXCLUSIVE clause" }
    block
      e(i) = a
      f(i) = b
    end block
  end do
end

subroutine f9
  use m
  implicit none
  integer i
! The first error (exit) causes two follow-up errors:
  !$omp simd reduction (inscan, +: a)  ! { dg-error "With INSCAN at .1., expected loop body with ..OMP SCAN between two structured-block-sequences" }
  do i = 1, 64
    if (i == 23) &
      exit  ! { dg-error "EXIT statement at .1. terminating ..OMP DO loop" } */
    !$omp scan exclusive (a) ! { dg-error "Unexpected ..OMP SCAN at .1. outside loop construct with 'inscan' REDUCTION clause" }
    a = a + 1
  end do
end
