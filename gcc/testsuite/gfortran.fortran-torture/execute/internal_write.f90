! PR 14901
! Internal writes were appending CR after the last char
! written by the format statement.
       CHARACTER*10 A
       WRITE(A,'(3HGCC)')
       IF (A.NE.'GCC       ') THEN
!         PRINT*,'A was not filled correctly by internal write'
!         PRINT*,' A = ',A
          CALL ABORT
       ENDIF
       END
