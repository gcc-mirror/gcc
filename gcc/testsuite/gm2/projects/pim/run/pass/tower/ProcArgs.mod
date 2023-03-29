IMPLEMENTATION MODULE ProcArgs ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Executive IMPORT SEMAPHORE, InitSemaphore, Wait, Signal ;

TYPE
   ProcessArgs = POINTER TO RECORD
                               argPtr: ADDRESS ;
                               taken,
                               given : SEMAPHORE ;
                            END ;

PROCEDURE InitArgs () : ProcessArgs ;
VAR
   p: ProcessArgs ;
BEGIN
   NEW(p) ;
   WITH p^ DO
      taken := InitSemaphore(1, 'ProcArgs') ;
      given := InitSemaphore(0, 'ProcArgs') ;
      argPtr := NIL
   END ;
   RETURN( p )
END InitArgs ;


PROCEDURE SetArgs (p: ProcessArgs; a: ADDRESS) : ADDRESS ;
BEGIN
   WITH p^ DO
      Wait(taken) ;
      argPtr := a ;
      Signal(given)
   END ;
   RETURN( NIL )
END SetArgs ;


PROCEDURE CollectArgs (p: ProcessArgs) : ADDRESS ;
VAR
   a: ADDRESS ;
BEGIN
   WITH p^ DO
      Wait(given) ;
      a := argPtr ;
      Signal(taken)
   END ;
   RETURN( a )
END CollectArgs ;


PROCEDURE KillArgs (p: ProcessArgs) : ProcessArgs ;
BEGIN
   DISPOSE(p) ;
   RETURN( NIL )
END KillArgs ;


END ProcArgs.
