MODULE simplepacked ;  

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT TBITSIZE, ROTATE ;

TYPE
   settype = SET OF [0..8] ;
   psettype = PACKEDSET OF [0..8] ;   


PROCEDURE assert (cond: BOOLEAN; line: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   IF NOT cond
   THEN
      printf ("assert failed %s at line %d\n", message, line) ;
      exit (1)
   END
END assert ;


PROCEDURE testset ;
VAR
   a, b: settype ;
BEGIN
   a := settype {1} ;
   b := a ;
   (* Assumes that the bitset will be contained in <= 64 bits, most likely
      32.  But probably safe to assume <= 64 bits for some time.  *)
   printf ("TBITSIZE (a) = %d\n", TBITSIZE (a));
   assert (TBITSIZE (a) <= 64, __LINE__, "TBITSIZE <= 64") ;
   assert (a = b, __LINE__, "comparision between variable sets") ;
   assert (a = settype {1}, __LINE__, "comparision between variable and constant sets") ;
   assert (b = settype {1}, __LINE__, "comparision between variable and constant sets") ;
   assert (settype {1} = settype {1}, __LINE__, "comparision between constant sets") ;
   assert (settype {1} # settype {2}, __LINE__, "comparision between constant sets") ;
   assert (ROTATE (settype {1}, 1) = ROTATE (settype {1}, 1), __LINE__, "comparision between constant rotated sets") ;
   assert (ROTATE (settype {1}, 1) # ROTATE (settype {2}, 1), __LINE__, "comparision between constant rotated sets") ;
   assert (ROTATE (a, 1) = settype {2}, __LINE__, "comparision between rotated variable and constant sets") ;
   assert (ROTATE (a, -1) = settype {0}, __LINE__, "comparision between rotated variable and constant sets") ;      
END testset ;


PROCEDURE testpset ;
VAR
   a, b: psettype ;
BEGIN
   a := psettype {1} ;
   b := a ;
   (* Packed set should be stored in a BYTE.  *)
   printf ("TBITSIZE (a) = %d\n", TBITSIZE (a));
   assert (TBITSIZE (a) <= 32, __LINE__, "TBITSIZE <= 32 ( packed set )") ;
   assert (a = b, __LINE__, "comparision between variable packed sets") ;
   assert (a = psettype {1}, __LINE__, "comparision between variable and constant packed sets") ;
   assert (b = psettype {1}, __LINE__, "comparision between variable and constant packed sets") ;
   assert (psettype {1} = psettype {1}, __LINE__, "comparision between constant packed sets") ;
   assert (psettype {1} # psettype {2}, __LINE__, "comparision between constant packed sets") ;
   assert (ROTATE (psettype {1}, 1) = ROTATE (psettype {1}, 1), __LINE__, "comparision between constant rotated packed sets") ;
   assert (ROTATE (psettype {1}, 1) # ROTATE (psettype {2}, 1), __LINE__, "comparision between constant rotated packed sets") ;
   assert (ROTATE (a, 1) = psettype {2}, __LINE__, "comparision between rotated variable and constant packed sets") ;
   assert (ROTATE (a, -1) = settype {0}, __LINE__, "comparision between rotated variable and constant packed sets") ;         
END testpset ;


BEGIN
   testset ;
   testpset
END simplepacked.
