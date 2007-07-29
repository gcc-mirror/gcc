! { dg-do run }
! Tests the fix for PR31211, in which the value of the result for
! cp_get_default_logger was stored as a temporary, rather than the
! pointer itself.  This caused a segfault when the result was
! nullified.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
  TYPE cp_logger_type
    INTEGER :: a
  END TYPE cp_logger_type

  if (cp_logger_log(cp_get_default_logger (0))) call abort ()
  if (.not. cp_logger_log(cp_get_default_logger (42))) call abort ()

CONTAINS

  logical function cp_logger_log(logger)
    TYPE(cp_logger_type), POINTER ::logger
    cp_logger_log = associated (logger) .and. (logger%a .eq. 42)
  END function

  FUNCTION cp_get_default_logger(v) RESULT(res)
    TYPE(cp_logger_type), POINTER ::res
    integer :: v
    if (v .eq. 0) then
      NULLIFY(RES)
    else
      allocate(RES)
      res%a = v
    end if
  END FUNCTION cp_get_default_logger
END
