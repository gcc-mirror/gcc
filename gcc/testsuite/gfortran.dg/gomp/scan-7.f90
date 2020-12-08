module m
  integer a, b
end module m

subroutine f2 (c, d, e, f)
  use m
  implicit none
  integer i, l, c(*), d(*), e(64), f(64)
  l = 1

  !$omp do reduction (inscan, +: a) linear (l)    ! { dg-error "'inscan' 'reduction' clause used together with 'linear' clause for a variable other than loop iterator" }
  do i = 1, 64
    block
      a = a + c(i)
      l = l + 1
    end block
    !$omp scan inclusive (a)
    d(i) = a
  end do 
end

subroutine f5 (c, d)
  use m
  implicit none
  integer i, c(64), d(64)
  !$omp simd reduction (inscan, +: a)
  do i = 1, 64
    d(i) = a
    !$omp scan exclusive (a, b)  ! { dg-error "'b' specified in 'exclusive' clause but not in 'inscan' 'reduction' clause on the containing construct" }
    a = a + c(i)
  end do
end

subroutine f6 (c, d)
  use m
  implicit none
  integer i, c(64), d(64)
  !$omp simd reduction (inscan, +: a, b)  ! { dg-error "'b' specified in 'inscan' 'reduction' clause but not in 'scan' directive clause" }
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
    if (i == 27) goto 123  ! { dg-error "invalid branch to/from OpenMP structured block" }
      ! { dg-warning "is not in the same block as the GOTO statement" "" { target *-*-* } .-1 }
    !$omp scan exclusive (a)
    block
123   a = 0  ! { dg-error "jump to label 'l1'" "" { target c++ } }
             ! { dg-warning "is not in the same block as the GOTO statement" "" { target *-*-* } .-1 }
    end block
  end do
end
