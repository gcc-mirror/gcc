! Verify column location information.

! See also 'c-c++-common/goacc/pr92793-1.c'.

! { dg-additional-options "-fdump-tree-original-lineno" }
! { dg-additional-options "-fdump-tree-gimple-lineno" }

! No tabs.  Funny indentation/spacing for a reason.


subroutine check ()
  implicit none (type, external)
  integer :: i, j, sum, diff

 !$acc    parallel &
     !$acc & & ! Fortran location information points to the last line, and last character of the directive.
!$acc  && ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:18:123\\\] #pragma acc parallel" 1 "original" } }
  !$acc & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:18:123\\\] #pragma omp target oacc_parallel" 1 "gimple" } }
      !$acc loop &
    !$acc & & ! Fortran location information points to the last line, and last character of the directive.
      !$acc  &   & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:26:22\\\] #pragma acc loop" 1 "original" } }
     !$acc &     & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:26:22\\\] #pragma acc loop" 1 "gimple" } }
    !$acc&       reduction  ( +    : sum ) & ! { dg-line sum1 }
 !$acc && ! Fortran location information points to the 's' in 'reduction(+:sum)'.
   !$acc   &    &  ! { dg-message "38: location of the previous reduction for 'sum'" "" { target *-*-* } sum1 }
!$acc&     independent
  do i = 1, 10
      !$acc loop &
!$acc & & ! Fortran location information points to the last line, and last character of the directive.
   !$acc & & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:36:34\\\] #pragma acc loop" 1 "original" } }
    !$acc & & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:36:34\\\] #pragma acc loop" 1 "gimple" } }
  !$acc & reduction(-: diff     ) &
             !$acc&reduction(- :    sum) & ! { dg-line sum2 }
            !$acc & & ! Fortran location information points to the ':' in 'reduction(-:sum)'.
          !$acc& & ! { dg-warning "37: conflicting reduction operations for 'sum'" "" { target *-*-* } sum2 }
          !$acc       &independent
     do j = 1, 10
           sum &
   & = &
      & 1
        ! Fortran location information points to the last line, and last character of the statement.
        ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:40:9\\\] sum = 1" 1 "original" } }
        ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:40:9\\\] sum = 1" 1 "gimple" } }
     end do
  end do
!$acc end  parallel
end subroutine check


subroutine gwv_sl_1 ()
  implicit none (type, external)
  integer :: i

  !$acc serial loop &
  !$acc &       gang(num:5) & ! { dg-error "25: argument not permitted on 'gang' clause" }
  !$acc &    worker(num:5) & ! { dg-error "24: argument not permitted on 'worker' clause" }
  !$acc &     vector(length:5) ! { dg-error "28: argument not permitted on 'vector' clause" }
  ! { dg-message "93: enclosing parent compute construct" "" { target *-*-* } .-1 }
  do i = 0, 10
  end do
  !$acc end serial loop
end subroutine gwv_sl_1

subroutine gwv_sl_2 ()
  implicit none (type, external)
  integer :: i, j, k

  !$acc serial loop ! { dg-message "77: enclosing parent compute construct" }
  do i = 0, 10
     !$acc loop ! { dg-bogus "enclosing parent compute construct" }
     do j = 0, 10
        !$acc loop &
        !$acc &           gang(num:5) & ! { dg-error "35: argument not permitted on 'gang' clause" }
        !$acc &      worker(num:5) & ! { dg-error "32: argument not permitted on 'worker' clause" }
        !$acc &    vector(length:5) ! { dg-error "33: argument not permitted on 'vector' clause" }
        do k = 0, 10
        end do
     end do
  end do
  !$acc end serial loop
end subroutine gwv_sl_2

subroutine gwv_s_l ()
  implicit none (type, external)
  integer :: i, j, k

  !$acc serial ! { dg-message "72: enclosing parent compute construct" }
  !$acc loop &
  !$acc &         gang(num:5) & ! { dg-error "27: argument not permitted on 'gang' clause" }
  !$acc &   worker(num:5) & ! { dg-error "23: argument not permitted on 'worker' clause" }
  !$acc &      vector(length:5) ! { dg-error "29: argument not permitted on 'vector' clause" }
  do i = 0, 10
  end do

  !$acc loop
  do i = 0, 10
     !$acc loop ! { dg-bogus "enclosing parent compute construct" }
     do j = 0, 10
        !$acc loop &
        !$acc &           gang(num:5) & ! { dg-error "35: argument not permitted on 'gang' clause" }
        !$acc &      worker(num:5) & ! { dg-error "32: argument not permitted on 'worker' clause" }
        !$acc &        vector(length:5) ! { dg-error "37: argument not permitted on 'vector' clause" }
        do k = 0, 10
        end do
     end do
  end do
!$acc end serial
end subroutine gwv_s_l

subroutine gwv_r () ! { dg-message "1: enclosing routine" }
  implicit none (type, external)
  integer :: i, j, k

  !$acc routine(gwv_r)

  !$acc loop &
  !$acc &     gang(num:5) & ! { dg-error "23: argument not permitted on 'gang' clause" }
  !$acc &      worker(num:5) & ! { dg-error "26: argument not permitted on 'worker' clause" }
  !$acc &    vector(length:5) ! { dg-error "27: argument not permitted on 'vector' clause" }
  do i = 0, 10
  end do

  !$acc loop
  do i = 0, 10
     !$acc loop
     do j = 0, 10
        !$acc loop &
        !$acc &       gang(num:5) & ! { dg-error "31: argument not permitted on 'gang' clause" }
        !$acc &     worker(num:5) & ! { dg-error "31: argument not permitted on 'worker' clause" }
        !$acc &       vector(length:5) ! { dg-error "36: argument not permitted on 'vector' clause" }
        do k = 0, 10
        end do
     end do
  end do
end subroutine gwv_r
