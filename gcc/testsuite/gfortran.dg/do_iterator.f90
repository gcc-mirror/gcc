! { dg-do compile }
! various checks which verify that we don't change do-iterators
DO I=1,5       ! { dg-error "cannot be redefined" "changing do-iterator 1" }
   I=1         ! { dg-error "cannot be redefined" "changing do-iterator 1" }
END DO
DO I=1,5       ! { dg-error "cannot be redefined" "changing do-iterator 2" }
   READ(5,*) I ! { dg-error "cannot be redefined" "changing do-iterator 2" }
END DO
DO I=1,5       ! { dg-error "cannot be redefined" "changing do-iterator 3" }
   READ(5,*,iostat=i) j ! { dg-error "cannot be redefined" "changing do-iterator 3" }
ENDDO
END
