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
 !$acc && ! Fortran location information points to the ':' in 'reduction(+:sum)'.
   !$acc   &    &  ! { dg-message "36: location of the previous reduction for 'sum'" "" { target *-*-* } sum1 }
!$acc&     independent
  do i = 1, 10
      !$acc loop &
!$acc & & ! Fortran location information points to the last line, and last character of the directive.
   !$acc & & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:36:34\\\] #pragma acc loop" 1 "original" } }
    !$acc & & ! { dg-final { scan-tree-dump-times "pr92793-1\\\.f90:36:34\\\] #pragma acc loop" 1 "gimple" } }
  !$acc & reduction(-: diff     ) &
             !$acc&reduction(- :    sum) & ! { dg-line sum2 }
            !$acc & & ! Fortran location information points to the ':' in 'reduction(-:sum)'.
          !$acc& & ! { dg-warning "32: conflicting reduction operations for 'sum'" "" { target *-*-* } sum2 }
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
