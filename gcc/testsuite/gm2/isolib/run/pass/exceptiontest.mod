MODULE exceptiontest;

IMPORT EXCEPTIONS;

VAR
   s: EXCEPTIONS.ExceptionSource;
BEGIN
   EXCEPTIONS.AllocateSource(s) ;
   EXCEPTIONS.RAISE(s, 1, 'Exception text')
EXCEPT
   HALT(0)
END exceptiontest.
