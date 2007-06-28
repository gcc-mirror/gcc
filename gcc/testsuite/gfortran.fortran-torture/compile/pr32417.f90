! PR tree-opt/32417
! this used to crash while running IV-opts
! aff_combination_add_elt was not ready to handle pointers correctly

SUBROUTINE ONEINTS()
  COMMON /INFOA / NAT,NUM
  DIMENSION TINT(NUM*NUM,NAT,3,3,3),TINTM(NUM,NUM,NAT,3,3,3)

  CALL TINTS(IC)
  DO ID=1,3
    DO IC=1,NAT
      TINTM(J,I,IC,IAN,INU,ID) = TINT((I-1)*NUM+J,IC,IAN,INU,ID)
    ENDDO
  ENDDO
END
