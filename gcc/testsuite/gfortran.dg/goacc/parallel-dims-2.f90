! Invalid use of OpenACC parallelism dimensions clauses: 'num_gangs',
! 'num_workers', 'vector_length'.

! See also '../../c-c++-common/goacc/parallel-dims-2.c'.

subroutine f()
  !TODO 'kernels', 'parallel' testing per '../../c-c++-common/goacc/parallel-dims-2.c'.
  !TODO This should incorporate some of the testing done in 'sie.f95'.


  ! The 'serial' construct doesn't allow these at all.

!$acc serial num_gangs (1)  ! { dg-error "Failed to match clause at" }
!$acc end serial  ! { dg-error "Unexpected !.ACC END SERIAL statement" }

!$acc serial num_workers (1)  ! { dg-error "Failed to match clause at" }
!$acc end serial  ! { dg-error "Unexpected !.ACC END SERIAL statement" }

!$acc serial vector_length (1)  ! { dg-error "Failed to match clause at" }
!$acc end serial  ! { dg-error "Unexpected !.ACC END SERIAL statement" }

end subroutine f
