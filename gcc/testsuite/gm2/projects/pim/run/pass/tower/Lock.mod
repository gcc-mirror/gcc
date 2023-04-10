IMPLEMENTATION MODULE Lock ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Executive IMPORT Wait, Signal, InitSemaphore,
                      SEMAPHORE ;


TYPE
   LOCK = POINTER TO RECORD
                        Mutex     : SEMAPHORE ;
                        ReadCount : CARDINAL ;
                        Wrt       : SEMAPHORE ;
                     END ;


PROCEDURE InitLock (Name: ARRAY OF CHAR) : LOCK ;
VAR
   l: LOCK ;
BEGIN
   NEW( l ) ;
   WITH l^ DO
      Mutex := InitSemaphore(1, Name) ;
      Wrt   := InitSemaphore(1, Name) ;
      ReadCount := 0
   END ;
   RETURN( l )
END InitLock ;


PROCEDURE GetReadAccess (l: LOCK) ;
BEGIN
   WITH l^ DO
      Wait( Mutex ) ;
      INC( ReadCount ) ;
      IF ReadCount=1
      THEN
         Wait( Wrt )
      END ;
      Signal( Mutex )
   END
END GetReadAccess ;


PROCEDURE ReleaseReadAccess (l: LOCK) ;
BEGIN
   WITH l^ DO
      Wait( Mutex ) ;
      DEC( ReadCount ) ;
      IF ReadCount=0
      THEN
         Signal( Wrt )
      END ;
      Signal( Mutex )
   END
END ReleaseReadAccess ;


PROCEDURE GetWriteAccess (l: LOCK) ;
BEGIN
   WITH l^ DO
      Wait( Wrt )
   END
END GetWriteAccess ;


PROCEDURE ReleaseWriteAccess (l: LOCK) ;
BEGIN
   WITH l^ DO
      Signal( Wrt )
   END
END ReleaseWriteAccess ;


END Lock.
