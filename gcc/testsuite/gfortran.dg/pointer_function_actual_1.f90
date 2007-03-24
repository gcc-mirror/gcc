! { dg-do run }
! Tests the fix for PR31209, in which an ICE would result because
! the reference to the pointer function f would be indirected, as
! if it were the result that is being passed.
!
! COntributed by Joost VandeVondele <jv244@cam.ac.uk>
!
FUNCTION F() RESULT(RES)
 INTEGER, POINTER :: RES
 ALLOCATE(RES)
 RES=2
END FUNCTION F

SUBROUTINE S1(f,*,*)
 INTERFACE
  FUNCTION F() RESULT(RES)
   INTEGER, POINTER :: RES
  END FUNCTION F
 END INTERFACE
 RETURN F()
END SUBROUTINE

PROGRAM TEST
   INTERFACE
    FUNCTION F() RESULT(RES)
     INTEGER, POINTER :: RES
    END FUNCTION F
   END INTERFACE


   INTERFACE
    SUBROUTINE S1(f,*,*)
      INTERFACE
       FUNCTION F() RESULT(RES)
        INTEGER, POINTER :: RES
       END FUNCTION F
      END INTERFACE
     END SUBROUTINE
   END INTERFACE

   CALL S1(F,*1,*2)

   1 CONTINUE
   CALL ABORT()

   GOTO 3
   2 CONTINUE

   3 CONTINUE
END

