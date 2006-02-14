! { dg-do compile }

        SUBROUTINE A25
        INTEGER OMP_GET_THREAD_NUM
        REAL A(20)
        INTEGER MYTHREAD
        !$OMP PARALLEL SHARED(A) PRIVATE(MYTHREAD)
        MYTHREAD = OMP_GET_THREAD_NUM()
        IF (MYTHREAD .EQ. 0) THEN
            CALL SUB(A(1:10)) ! compiler may introduce writes to A(6:10)
        ELSE
            A(6:10) = 12
        ENDIF
        !$OMP END PARALLEL
        END SUBROUTINE A25
        SUBROUTINE SUB(X)
        REAL X(*)
        X(1:5) = 4
        END SUBROUTINE SUB
