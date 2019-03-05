! { dg-do compile }
! { dg-additional-options "-Wcharacter-truncation" }
! PR 82743 - warnings were missing on truncation of structure
! constructors.
! Original test case by Simon Klüpfel
PROGRAM TEST
    TYPE A
        CHARACTER(LEN=1) :: C
    END TYPE A
    TYPE(A) :: A1
    A1=A("123") ! { dg-warning "CHARACTER expression will be truncated" }
    A1=A(C="123") ! { dg-warning "CHARACTER expression will be truncated" }
    A1%C="123" ! { dg-warning "CHARACTER expression will be truncated" }
END PROGRAM TEST
