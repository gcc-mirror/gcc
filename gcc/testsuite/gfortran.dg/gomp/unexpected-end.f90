! PR fortran/102313

!$omp end ATOMIC  ! { dg-error "Unexpected !.OMP END ATOMIC" }

!$omp end CRITICAL  ! { dg-error "Unexpected !.OMP END CRITICAL" }

!$omp end DISTRIBUTE  ! { dg-error "Unexpected !.OMP END DISTRIBUTE" }

!$omp end DISTRIBUTE PARALLEL DO  ! { dg-error "Unexpected !.OMP END DISTRIBUTE PARALLEL DO" }

!$omp end DISTRIBUTE PARALLEL DO SIMD  ! { dg-error "Unexpected !.OMP END DISTRIBUTE PARALLEL DO SIMD" }

!$omp end DISTRIBUTE SIMD  ! { dg-error "Unexpected !.OMP END DISTRIBUTE SIMD" }

!$omp end DO  ! { dg-error "Unexpected !.OMP END DO" }

!$omp end DO SIMD  ! { dg-error "Unexpected !.OMP END DO SIMD" }

!$omp end LOOP  ! { dg-error "Unclassifiable OpenMP directive" }

!$omp parallel loop
do i = 1, 5
end do
!$omp end LOOP  ! { dg-error "Unclassifiable OpenMP directive" }

!$omp end MASKED  ! { dg-error "Unexpected !.OMP END MASKED" }

!$omp end MASKED TASKLOOP  ! { dg-error "Unexpected !.OMP END MASKED TASKLOOP" }

!$omp end MASKED TASKLOOP SIMD  ! { dg-error "Unexpected !.OMP END MASKED TASKLOOP SIMD" }

!$omp end MASTER  ! { dg-error "Unexpected !.OMP END MASTER" }

!$omp end MASTER TASKLOOP  ! { dg-error "Unexpected !.OMP END MASTER TASKLOOP" }

!$omp end MASTER TASKLOOP SIMD  ! { dg-error "Unexpected !.OMP END MASTER TASKLOOP SIMD" }

!$omp end ORDERED  ! { dg-error "Unexpected !.OMP END ORDERED" }

!$omp end PARALLEL  ! { dg-error "Unexpected !.OMP END PARALLEL" }

!$omp end PARALLEL DO  ! { dg-error "Unexpected !.OMP END PARALLEL DO" }

!$omp end PARALLEL DO SIMD  ! { dg-error "Unexpected !.OMP END PARALLEL DO SIMD" }

!$omp loop
!$omp end PARALLEL LOOP  ! { dg-error "Unexpected junk" }

!$omp end PARALLEL MASKED  ! { dg-error "Unexpected !.OMP END PARALLEL MASKED" }

!$omp end PARALLEL MASKED TASKLOOP  ! { dg-error "Unexpected !.OMP END PARALLEL MASKED TASKLOOP" }

!$omp end PARALLEL MASKED TASKLOOP SIMD  ! { dg-error "Unexpected !.OMP END PARALLEL MASKED TASKLOOP SIMD" }

!$omp end PARALLEL MASTER  ! { dg-error "Unexpected !.OMP END PARALLEL MASTER" }

!$omp end PARALLEL MASTER TASKLOOP  ! { dg-error "Unexpected !.OMP END PARALLEL MASTER TASKLOOP" }

!$omp end PARALLEL MASTER TASKLOOP SIMD  ! { dg-error "Unexpected !.OMP END PARALLEL MASTER TASKLOOP SIMD" }

!$omp end PARALLEL SECTIONS  ! { dg-error "Unexpected !.OMP END PARALLEL SECTIONS" }

!$omp end PARALLEL WORKSHARE  ! { dg-error "Unexpected !.OMP END PARALLEL WORKSHARE" }

!$omp end SCOPE  ! { dg-error "Unexpected !.OMP END SCOPE" }

!$omp end SECTIONS  ! { dg-error "Unexpected !.OMP END SECTIONS" }

!$omp end SIMD  ! { dg-error "Unexpected !.OMP END SIMD" }

!$omp end SINGLE  ! { dg-error "Unexpected !.OMP END SINGLE" }

!$omp end TARGET  ! { dg-error "Unexpected !.OMP END TARGET" }

!$omp end TARGET DATA  ! { dg-error "Unexpected !.OMP END TARGET DATA" }

!$omp end TARGET PARALLEL  ! { dg-error "Unexpected !.OMP END TARGET PARALLEL" }

!$omp end TARGET PARALLEL DO  ! { dg-error "Unexpected !.OMP END TARGET PARALLEL DO" }

!$omp end TARGET PARALLEL DO SIMD  ! { dg-error "Unexpected !.OMP END TARGET PARALLEL DO SIMD" }

!$omp end TARGET PARALLEL LOOP  ! { dg-error "Unexpected junk" }

!$omp end TARGET SIMD  ! { dg-error "Unexpected !.OMP END TARGET SIMD" }

!$omp end TARGET TEAMS  ! { dg-error "Unexpected !.OMP END TARGET TEAMS" }

!$omp end TARGET TEAMS DISTRIBUTE  ! { dg-error "Unexpected !.OMP END TARGET TEAMS DISTRIBUTE" }

!$omp end TARGET TEAMS DISTRIBUTE PARALLEL DO  ! { dg-error "Unexpected !.OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO" }

!$omp end TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD  ! { dg-error "Unexpected !.OMP END TARGET TEAMS DISTRIBUTE PARALLEL DO SIMD" }

!$omp end TARGET TEAMS DISTRIBUTE SIMD  ! { dg-error "Unexpected !.OMP END TARGET TEAMS DISTRIBUTE SIMD" }

!$omp end TARGET TEAMS LOOP  ! { dg-error "Unexpected junk" }

!$omp end TASK  ! { dg-error "Unexpected !.OMP END TASK" }

!$omp end TASKGROUP  ! { dg-error "Unexpected !.OMP END TASKGROUP" }

!$omp end TASKLOOP  ! { dg-error "Unexpected !.OMP END TASKLOOP" }

!$omp end TASKLOOP SIMD  ! { dg-error "Unexpected !.OMP END TASKLOOP SIMD" }

!$omp end TEAMS  ! { dg-error "Unexpected !.OMP END TEAMS" }

!$omp end TEAMS DISTRIBUTE  ! { dg-error "Unexpected !.OMP END TEAMS DISTRIBUTE" }

!$omp end TEAMS DISTRIBUTE PARALLEL DO  ! { dg-error "Unexpected !.OMP END TEAMS DISTRIBUTE PARALLEL DO" }

!$omp end TEAMS DISTRIBUTE PARALLEL DO SIMD  ! { dg-error "Unexpected !.OMP END TEAMS DISTRIBUTE PARALLEL DO SIMD" }

!$omp end TEAMS DISTRIBUTE SIMD  ! { dg-error "Unexpected !.OMP END TEAMS DISTRIBUTE SIMD" }

!$omp end TEAMS LOOP  ! { dg-error "Unexpected junk" }

!$omp end WORKSHARE  ! { dg-error "Unexpected !.OMP END WORKSHARE" }

end  ! { dg-error "Unexpected END statement" }

! { dg-excess-errors "Unexpected end of file" }
