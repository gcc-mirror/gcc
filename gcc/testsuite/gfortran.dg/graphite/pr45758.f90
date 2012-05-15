! { dg-options "-O3 -floop-block" }

MODULE util
  INTEGER, PARAMETER :: int_4=4
  INTERFACE sort
     MODULE PROCEDURE sort_int_4v
  END INTERFACE
CONTAINS
  SUBROUTINE sort_int_4v ( arr, n, index )
    INTEGER(KIND=int_4), INTENT(INOUT)       :: arr(1:n)
    INTEGER, INTENT(OUT)                     :: INDEX(1:n)
    DO i = 1, n
       INDEX(i) = i
    END DO
1   IF (ir-l<m) THEN
       DO j = l + 1, ir
          DO i = j - 1, 1, -1
             IF (arr(i)<=a) GO TO 2
             arr(i+1) = arr(i)
             INDEX(i+1) = INDEX(i)
          END DO
2         arr(i+1) = a
       END DO
    END IF
  END SUBROUTINE sort_int_4v
  SUBROUTINE create_destination_list(list)
    INTEGER, DIMENSION(:, :, :), POINTER     :: list
    INTEGER                                  ::  icpu, ncpu, stat, ultimate_max
    INTEGER, ALLOCATABLE, DIMENSION(:)       :: index, sublist
    ultimate_max=7
    ALLOCATE(INDEX(ultimate_max),STAT=stat)
    CALL t(stat==0)
    ALLOCATE(sublist(ultimate_max),STAT=stat)
    DO icpu=0,ncpu-1
       CALL sort(sublist,ultimate_max,index)
       list(1,:,icpu)=sublist
       list(2,:,icpu)=0
    ENDDO
  END SUBROUTINE create_destination_list
END MODULE
