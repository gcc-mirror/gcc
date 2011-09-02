MODULE cp_parser_methods
  INTEGER, PARAMETER :: default_string_length=80
  INTEGER, PARAMETER :: default_path_length=250
  TYPE ilist_type
     LOGICAL                              :: in_use
  END TYPE ilist_type
  TYPE cp_parser_type
     CHARACTER(LEN=default_path_length)             :: ifn
     INTEGER                                        :: icol,icol1,icol2
     TYPE(ilist_type), POINTER                      :: ilist
  END TYPE cp_parser_type
  TYPE cp_error_type
  END TYPE cp_error_type
CONTAINS
  FUNCTION cts(i) RESULT(res)
    CHARACTER(len=6)                         :: res
  END FUNCTION cts
  FUNCTION parser_location(parser,error) RESULT(res)
    TYPE(cp_parser_type), POINTER            :: parser
    TYPE(cp_error_type), INTENT(inout)       :: error
    CHARACTER(len=default_path_length+default_string_length)       :: res
    LOGICAL                                  :: failure
    IF (.NOT. failure) THEN
       res="file:'"//TRIM(parser%ifn)//"' line:"//cts(parser%icol)
    END IF
  END FUNCTION parser_location
  SUBROUTINE parser_get_integer(parser,at_end, error)
    TYPE(cp_parser_type), POINTER            :: parser
    TYPE(cp_error_type), INTENT(inout)       :: error
    LOGICAL                                  :: failure, my_at_end
    IF (.NOT.failure) THEN
       IF (.NOT.parser%ilist%in_use) THEN
          CALL cp_assert("A"// TRIM(parser_location(parser,error)))
       END IF
    END IF
  END SUBROUTINE parser_get_integer
  SUBROUTINE parser_get_string(parser,at_end,error)
    TYPE(cp_parser_type), POINTER            :: parser
    LOGICAL, INTENT(out), OPTIONAL           :: at_end
    TYPE(cp_error_type), INTENT(inout)       :: error
    LOGICAL                                  :: failure, my_at_end
    IF (.NOT.failure) THEN
       IF (PRESENT(at_end)) THEN
          CALL cp_assert("s"//TRIM(parser_location(parser,error)))
       END IF
    END IF
  END SUBROUTINE parser_get_string
END MODULE cp_parser_methods
