PROGRAM nested_gwv
CONTAINS
  SUBROUTINE gwv
    INTEGER  :: i
    REAL(KIND=8), ALLOCATABLE :: un(:),  ua(:)

    !$acc kernels num_gangs(2) num_workers(4) vector_length(32)
    DO jj = 1, 100
       un(i) = ua(i)
    END DO
    !$acc end kernels

    !$acc parallel num_gangs(2) num_workers(4) vector_length(32)
    DO jj = 1, 100
       un(i) = ua(i)
    END DO
    !$acc end parallel
  END SUBROUTINE gwv
END PROGRAM nested_gwv
