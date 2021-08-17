subroutine foo

!$omp do
do i = 1, 2
end do
!$omp end do nowait foo  ! { dg-error "Unexpected junk after NOWAIT clause" }
!$omp end do  ! as previous line is ignored

!$omp scope
  block; end block
!$omp end scope bar  ! { dg-error "Unexpected junk at" }
!$omp end scope

!$omp scope
  block; end block
!$omp end scope nowait nowait ! { dg-error "Unexpected junk after NOWAIT clause" }
!$omp end scope

end
