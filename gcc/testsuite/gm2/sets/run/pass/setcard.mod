MODULE setcard ;

FROM libc IMPORT exit ;

TYPE
   large = SET OF CARDINAL ;
VAR
   set: large ;
BEGIN
   set := large {} ;
   INCL (set, 2) ;
   IF 2 IN set
   THEN
      exit (0)
   ELSE
      exit (1)
   END
END setcard.
