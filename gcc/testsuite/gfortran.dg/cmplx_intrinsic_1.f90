! { dg-do compile }

CONTAINS
SUBROUTINE send_forward ()

    INTEGER, DIMENSION(3)                    :: lz, ub, uz
    REAL, ALLOCATABLE, DIMENSION(:, :, :)    :: buffer
    COMPLEX, DIMENSION ( :, :, : ), POINTER  :: cc3d

    cc3d ( lz(1):uz(1), lz(2):uz(2), lz(3):uz(3) ) = &
           CMPLX ( buffer ( lz(1):uz(1), lz(2):uz(2), lz(3):uz(3) ), &
                   KIND = SELECTED_REAL_KIND ( 14, 200 ) )
   
END SUBROUTINE send_forward
END

