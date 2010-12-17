! { dg-do compile }
! { dg-options "-O2 -ftree-vectorize" }

      TYPE :: spot_weld_type                                            
        CHARACTER(8)     PLACE      ! Keyword "NODE" or "POSITION"         
      END TYPE                                                          
      TYPE (spot_weld_type),  DIMENSION(:), ALLOCATABLE :: SPOT_WELD    
      INTEGER, PARAMETER :: LSRT = 12 ! Length of sorted-element-distance array
      INTEGER                    &
     &          IETYP(LSRT)        ! -/- Sort array for closest el's, 0/1=tri/qu
      REAL(KIND(0D0))                                                          &
     &          DSQRD(LSRT)        ! -/- Sort array for closest el's, d**2
      LOGICAL                                                                  &
     &          COINCIDENT,                                                    &
     &          INSIDE_ELEMENT
      IF (SPOT_WELD(NSW)%PLACE .EQ. 'POSITION') THEN
        DO n = 1,LSRT
        ENDDO
        DO i = 1,NUMP3
          DO WHILE (Distance_Squared .GT. DSQRD(n) .AND. n .LE. LSRT)
          ENDDO
          IF (n .LT. LSRT) THEN
            DO k = LSRT-1,n,-1
              DSQRD(k+1) = DSQRD(k)
              IETYP(k+1) = IETYP(k)
            ENDDO
          ENDIF
          DO n = 1,LSRT
            IF (IETYP(n) .EQ. 0) THEN
              INSIDE_ELEMENT =                                                 &
     &          Xi1EL(n) .GE. 0.0 .AND. Xi2EL(n) .GE. 0.0
              IF (DSQRD(n) .LT. Dmin) THEN
              ENDIF
            ENDIF
          ENDDO
        ENDDO
        IF (Icount .GT. 0) THEN
          DO i = 1,Icount
            CALL USER_MESSAGE                                                  &
     &          (                                                              &
     &          )
          ENDDO
          CALL USER_MESSAGE                                                    &
     &          (                                                              &
     &          )
        ENDIF
        IF                                                                     &
     &          (                                                              &
     &          .NOT.COINCIDENT                                                &
     &          )                                                              &
     &    THEN
          IF (NP1 .GT. 0) THEN
            IF (NP1 .GT. 0) THEN
            ENDIF
          ENDIF
        ENDIF
        IF (.NOT.COINCIDENT) THEN
          DO i = 1,3
            IF (NP(i) .GT. 0) THEN
            ENDIF
          ENDDO
        ENDIF
      ENDIF
      END
