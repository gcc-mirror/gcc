MODULE input_cp2k_motion
  IMPLICIT NONE
  interface
    SUBROUTINE keyword_create(variants)
      CHARACTER(len=*), DIMENSION(:), &
      INTENT(in)                   :: variants
    end subroutine
  end interface
CONTAINS
  SUBROUTINE create_neb_section()
    CALL keyword_create(variants=(/"K"/))
  END SUBROUTINE create_neb_section
END MODULE input_cp2k_motion

