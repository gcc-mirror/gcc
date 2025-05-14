MODULE whilestep ;  

FROM libc IMPORT printf ;

PROCEDURE init ;
VAR
   c: CARDINAL ;
BEGIN
   c := 0 ;
   WHILE c < 20 DO
      printf ("iteration %d\n", c) ;
      INC (c)
   END
END init ;

BEGIN
   init
END whilestep.
