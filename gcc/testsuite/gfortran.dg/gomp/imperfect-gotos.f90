! This test case is expected to fail due to errors.

! These jumps are all OK since they are to/from the same structured block.
subroutine f1 ()
  integer :: i, j
  !$omp do collapse(2) 
  do i = 1, 64
    go to 10
10  continue
    do j = 1, 64
      go to 11
11    continue
    end do
    go to 12
12  continue    
  end do
end subroutine

! Jump around loop body to/from different structured blocks of intervening
! code.
subroutine f2 ()
  integer :: i, j
  !$omp do collapse(2) 
  do i = 1, 64
    go to 20
20  continue
    if (i > 16) go to 22 ! { dg-error "invalid branch to/from OpenMP structured block" }
    do j = 1, 64
      go to 21
21    continue
    end do
    go to 22
22  continue    
  end do
end subroutine

! Jump into loop body from intervening code.
subroutine f3 ()
  integer :: i, j
  !$omp do collapse(2) 
  do i = 1, 64
    go to 30
30  continue
    if (i > 16) go to 31 ! { dg-error "invalid branch to/from OpenMP structured block" }
    ! { dg-warning "Legacy Extension:" "" { target *-*-* } .-1 }
    do j = 1, 64
      go to 31
31    continue  ! { dg-warning "Legacy Extension:" }
    end do
    go to 32
32  continue    
  end do
end subroutine

! Jump out of loop body to intervening code.
subroutine f4 ()
  integer :: i, j
  !$omp do collapse(2) 
  do i = 1, 64
    go to 40
40  continue
    do j = 1, 64
      if (i > 16) go to 41 ! { dg-error "invalid branch to/from OpenMP structured block" }
    end do
41  continue
    go to 42
42  continue    
  end do
end subroutine
