! { dg-do run }
! PR122936, derived from the original provided by the reporter.
! Before the patch this gave a runtime error.
module test_io
    TYPE :: MYTYPE
        REAL :: value
    END TYPE
    INTERFACE read(formatted)
        MODULE PROCEDURE read_formatted
    END INTERFACE
    PUBLIC :: read(formatted)
contains
    ! Formatted Input
    SUBROUTINE read_formatted(dtv, unit, iotype, v_list, iostat, iomsg)
        CLASS(MYTYPE), INTENT(INOUT) :: dtv
        INTEGER, INTENT(IN)      :: unit
        CHARACTER(*), INTENT(IN) :: iotype
        INTEGER, INTENT(IN)      :: v_list(:)
        INTEGER, INTENT(OUT)     :: iostat
        CHARACTER(*), INTENT(INOUT) :: iomsg

        REAL   :: tmp

        READ(unit, FMT = *, IOSTAT=iostat, IOMSG=iomsg) tmp
        IF (iostat == 0) dtv%value = tmp
    END SUBROUTINE read_formatted

end module

PROGRAM MAIN
    USE test_io
    INTEGER, PARAMETER  :: NIN = 15
    TYPE(MYTYPE)       :: V11, V12, V13
    INTEGER            :: V21, V22, V23
    OPEN(NIN, status='scratch')
    WRITE(NIN,*) "    2.5 9 1.5, AValue for V1"
    WRITE(NIN,*) "    15 2.4 17,  BValue for V2"
    REWIND(NIN)
    READ(NIN, FMT = *) V11, V23, V12
    READ(NIN, FMT = *) V21, V13, V22
    CLOSE(NIN)
END PROGRAM MAIN

