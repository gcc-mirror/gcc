! { dg-do compile }
! { dg-options "-O1" }
SUBROUTINE check_for_overlap (cell_length)
  REAL, DIMENSION(1:3), INTENT(IN), OPTIONAL :: cell_length
  REAL, DIMENSION(1:3) :: abc, box_length

  IF (PRESENT(cell_length)) THEN
    box_length(1:3)=abc(1:3)
  ENDIF
END SUBROUTINE check_for_overlap
