! { dg-do run }
! { dg-additional-options "-fbounds-check" }
MODULE cp_units

  INTEGER, PARAMETER :: default_string_length=80, dp=KIND(0.0D0)

  LOGICAL, PRIVATE, PARAMETER          :: debug_this_module=.TRUE.
  CHARACTER(len=*), PARAMETER, PRIVATE :: moduleN = 'cp_units'
  INTEGER, SAVE, PRIVATE               :: last_unit_id=0, last_unit_set_id=0

  INTEGER, PARAMETER, PUBLIC :: cp_unit_max_kinds=8, cp_unit_basic_desc_length=15,&
       cp_unit_desc_length=cp_unit_max_kinds*cp_unit_basic_desc_length, cp_ukind_max=9

CONTAINS

  FUNCTION cp_to_string(i) RESULT(res)
    INTEGER, INTENT(in)                      :: i
    CHARACTER(len=6)                         :: res

    INTEGER                                  :: iostat
    REAL(KIND=dp)                            :: tmp_r

    IF (i>999999 .OR. i<-99999) THEN
       tmp_r=i
       WRITE (res,fmt='(es6.1)',iostat=iostat) tmp_r
    ELSE
       WRITE (res,fmt='(i6)',iostat=iostat) i
    END IF
    IF (iostat/=0) THEN
       STOP 7
    END IF
  END FUNCTION cp_to_string

  SUBROUTINE cp_unit_create(string)
    CHARACTER(len=*), INTENT(in)             :: string

    CHARACTER(len=*), PARAMETER :: routineN = 'cp_unit_create', &
      routineP = moduleN//':'//routineN

    CHARACTER(default_string_length)         :: desc
    CHARACTER(LEN=40)                        :: formatstr
    INTEGER                                  :: i_high, i_low, i_unit, &
                                                len_string, next_power
    INTEGER, DIMENSION(cp_unit_max_kinds)    :: kind_id, power, unit_id
    LOGICAL                                  :: failure

    failure=.FALSE.
    unit_id=cp_units_none
    kind_id=cp_ukind_none
    power=0
    i_low=1
    i_high=1
    len_string=LEN(string)
    i_unit=0
    next_power=1
    DO WHILE(i_low<len_string)
       IF (string(i_low:i_low)/=' ') EXIT
       i_low=i_low+1
    END DO
    i_high=i_low
    DO WHILE(i_high<=len_string)
       IF ( string(i_high:i_high)==' '.OR.string(i_high:i_high)=='^'.OR.&
            string(i_high:i_high)=='*'.OR.string(i_high:i_high)=='/') EXIT
       i_high=i_high+1
    END DO
    DO WHILE(.NOT.failure)
       IF (i_high<=i_low.OR.i_low>len_string) EXIT
       i_unit=i_unit+1
       IF (i_unit>cp_unit_max_kinds) THEN
          EXIT
       END IF
       power(i_unit)=next_power
       ! parse op
       i_low=i_high
       DO WHILE(i_low<=len_string)
          IF (string(i_low:i_low)/=' ') EXIT
          i_low=i_low+1
       END DO
       i_high=i_low
       DO WHILE(i_high<=len_string)
          IF ( string(i_high:i_high)==' '.OR.string(i_high:i_high)=='^'.OR.&
               string(i_high:i_high)=='*'.OR.string(i_high:i_high)=='/') EXIT
          i_high=i_high+1
       END DO
       IF (i_high<i_low.OR.i_low>len_string) EXIT

       IF (i_high<=len_string) THEN
          IF (string(i_low:i_high)=='^') THEN
             i_low=i_high+1
             DO WHILE(i_low<=len_string)
                IF (string(i_low:i_low)/=' ') EXIT
                i_low=i_low+1
             END DO
             i_high=i_low
             DO WHILE(i_high<=len_string)
                SELECT CASE(string(i_high:i_high))
                CASE('+','-','0','1','2','3','4','5','6','7','8','9')
                   i_high=i_high+1
                CASE default
                   EXIT
                END SELECT
             END DO
             IF (i_high<=i_low.OR.i_low>len_string) THEN
                write(6,*) "BUG : XXX"//string//"XXX integer expected"
                STOP 1
                EXIT
             END IF
          END IF
       ENDIF
    END DO
  END SUBROUTINE cp_unit_create

END MODULE cp_units

USE cp_units
CALL cp_unit_create("fs^-1")
END
