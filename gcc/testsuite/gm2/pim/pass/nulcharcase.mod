MODULE nulcharcase ;

FROM libc IMPORT printf ;

VAR
   ch: CHAR;
BEGIN
   ch := '';
   CASE ch OF

   '' : printf ("null char seen\n") |
   '1': printf ("1\n")

   ELSE
   END
END nulcharcase.
