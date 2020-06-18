! { dg-do compile }

SUBROUTINE foo(n,array)
    IMPLICIT NONE
    INTEGER, INTENT (IN) :: n
    REAL, INTENT(INOUT),OPTIONAL:: array(:)
    INTEGER:: i

    !$OMP PARALLEL DO DEFAULT(none) SHARED(array,n) PRIVATE(i)
    DO i = 1,n
       IF (PRESENT(array)) THEN
          array(i) = array(i) + i
       ENDIF
    ENDDO
    !$OMP END PARALLEL DO
END SUBROUTINE foo

subroutine s1 (array)
  real, optional :: array(:)
  !$omp parallel default(none) firstprivate (array)
  if (present (array)) array(:) = 3
  !$omp end parallel
end subroutine

subroutine s2 (array)
  real, optional :: array(:)
  !$omp parallel default(none) shared (array)
  !$omp master
  if (present (array)) array(:) = 3
  !$omp end master
  !$omp end parallel
end subroutine

subroutine s3 (array)
  real, optional :: array(:)
  !$omp parallel default(none) private (array)
  if (present (array)) array(:) = 3
  !$omp end parallel
end subroutine

subroutine s4 (arg)
  real, optional :: arg
  !$omp parallel default(none) firstprivate (arg)
  if (present (arg)) arg = 3
  !$omp end parallel
end subroutine

subroutine s5 (arg)
  real, optional :: arg
  !$omp parallel default(none) shared (arg)
  !$omp master
  if (present (arg)) arg = 3
  !$omp end master
  !$omp end parallel
end subroutine

subroutine s6 (arg)
  real, optional :: arg
  !$omp parallel default(none) private (arg)
  if (present (arg)) arg = 3
  !$omp end parallel
end subroutine

subroutine s7 (arg)
  real, value, optional :: arg
  !$omp parallel default(none) firstprivate (arg)
  if (present (arg)) arg = 3
  !$omp end parallel
end subroutine

subroutine s8 (arg)
  real, value, optional :: arg
  !$omp parallel default(none) shared (arg)
  !$omp master
  if (present (arg)) arg = 3
  !$omp end master
  !$omp end parallel
end subroutine

subroutine s9 (arg)
  real, value, optional :: arg
  !$omp parallel default(none) private (arg)
  if (present (arg)) arg = 3
  !$omp end parallel
end subroutine

subroutine s10 (arg)
  real, optional :: arg(..)
  !$omp parallel default(none) private (arg)
  if (present (arg)) stop 10
  !$omp end parallel
end subroutine

subroutine w1 (array)
  real, optional :: array(:)
  !$omp parallel default(none)     ! { dg-message "note: enclosing 'parallel'" }
  if (.not.present (array)) stop 1 ! { dg-error "'array' not specified in enclosing 'parallel'" }
  !$omp end parallel
end subroutine

subroutine w2 (array2)
  real, optional :: array2(*)
  !$omp parallel default(none)      ! { dg-message "note: enclosing 'parallel'" "TODO" { xfail *-*-* } }
  if (.not.present (array2)) stop 2 ! { dg-error "'array2' not specified in enclosing 'parallel'" "TODO" { xfail *-*-* } }
  !$omp end parallel
end subroutine

subroutine w3 (arg)
  real, optional :: arg
  !$omp parallel default(none)    ! { dg-message "note: enclosing 'parallel'" }
  if (.not.present (arg)) stop 3  ! { dg-error "'arg' not specified in enclosing 'parallel'" }
  !$omp end parallel
end subroutine

subroutine w4 (arg2)
  real, value, optional :: arg2
  !$omp parallel default(none)     ! { dg-message "note: enclosing 'parallel" "TODO" { xfail *-*-* } }
  if (.not.present (arg2)) stop 4  ! { dg-error "'arg2' not specified in enclosing 'parallel'" "TODO" { xfail *-*-*} }
  !$omp end parallel
end subroutine

subroutine w5 (array3)
  real, optional :: array3(..)
  !$omp parallel default(none)      ! { dg-message "note: enclosing 'parallel'" }
  if (.not.present (array3)) stop 5 ! { dg-error "'array3' not specified in enclosing 'parallel'" }
  !$omp end parallel
end subroutine
