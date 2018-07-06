! { dg-do run }
! { dg-additional-sources proc_ptr_8.c }
!
! PR fortran/32580
! Original test case
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>

MODULE X

  USE ISO_C_BINDING
  INTERFACE
    INTEGER(KIND=C_INT) FUNCTION mytype( a ) BIND(C)
       USE ISO_C_BINDING
       INTEGER(KIND=C_INT), VALUE :: a
    END FUNCTION
    SUBROUTINE init() BIND(C,name="init")
    END SUBROUTINE
  END INTERFACE

  TYPE(C_FUNPTR), BIND(C,name="funpointer") :: funpointer

END MODULE X

USE X
PROCEDURE(mytype), POINTER :: ptype,ptype2

CALL init()
CALL C_F_PROCPOINTER(funpointer,ptype)
if (ptype(3_c_int) /= 9) STOP 1

! the stuff below was added with PR 42072
call setpointer(ptype2)
if (ptype2(4_c_int) /= 12) STOP 2

contains

  subroutine setpointer (p)
    PROCEDURE(mytype), POINTER :: p
    CALL C_F_PROCPOINTER(funpointer,p)
  end subroutine

END
