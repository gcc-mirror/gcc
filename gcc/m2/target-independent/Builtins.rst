
.. code-block:: modula2
    DEFINITION MODULE Builtins ;

    FROM SYSTEM IMPORT ADDRESS ;
    
    (* floating point intrinsic procedure functions *)
    
.. index::
   isfinitef
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinitef (x: SHORTREAL) : BOOLEAN ;
.. index::
   isfinite
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinite (x: REAL) : BOOLEAN ;
.. index::
   isfinitel
.. code-block:: modula2
    PROCEDURE __BUILTIN__ isfinitel (x: LONGREAL) : BOOLEAN ;
    
.. index::
   sinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sinf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sin (x: REAL) : REAL ;
.. index::
   sinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sinl (x: LONGREAL) : LONGREAL ;
    
.. index::
   cosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cosf (x: SHORTREAL) : SHORTREAL ;
.. index::
   cos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cos (x: REAL) : REAL ;
.. index::
   cosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cosl (x: LONGREAL) : LONGREAL ;
    
.. index::
   sqrtf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrtf (x: SHORTREAL) : SHORTREAL ;
.. index::
   sqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrt (x: REAL) : REAL ;
.. index::
   sqrtl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ sqrtl (x: LONGREAL) : LONGREAL ;
    
.. index::
   atan2f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2f (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   atan2
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2 (x, y: REAL) : REAL ;
.. index::
   atan2l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ atan2l (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   fabsf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabsf (x: SHORTREAL) : SHORTREAL ;
.. index::
   fabs
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabs (x: REAL) : REAL ;
.. index::
   fabsl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ fabsl (x: LONGREAL) : LONGREAL ;
    
.. index::
   logf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ logf (x: SHORTREAL) : SHORTREAL ;
.. index::
   log
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log (x: REAL) : REAL ;
.. index::
   logl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ logl (x: LONGREAL) : LONGREAL ;
    
.. index::
   expf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ expf (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp (x: REAL) : REAL ;
.. index::
   expl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ expl (x: LONGREAL) : LONGREAL ;
    
.. index::
   log10f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   log10
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10 (x: REAL) : REAL ;
.. index::
   log10l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ log10l (x: LONGREAL) : LONGREAL ;
    
.. index::
   exp10f
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10f (x: SHORTREAL) : SHORTREAL ;
.. index::
   exp10
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10 (x: REAL) : REAL ;
.. index::
   exp10l
.. code-block:: modula2
    PROCEDURE __BUILTIN__ exp10l (x: LONGREAL) : LONGREAL ;
    
.. index::
   ilogbf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogbf (x: SHORTREAL) : INTEGER ;
.. index::
   ilogb
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogb (x: REAL) : INTEGER ;
.. index::
   ilogbl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ilogbl (x: LONGREAL) : INTEGER ;
    
.. index::
   huge_val
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_val () : REAL ;
.. index::
   huge_valf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_valf () : SHORTREAL ;
.. index::
   huge_vall
.. code-block:: modula2
    PROCEDURE __BUILTIN__ huge_vall () : LONGREAL ;
    
.. index::
   significand
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significand (r: REAL) : REAL ;
.. index::
   significandf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significandf (s: SHORTREAL) : SHORTREAL ;
.. index::
   significandl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ significandl (l: LONGREAL) : LONGREAL ;
    
.. index::
   modf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modf (x: REAL; VAR y: REAL) : REAL ;
.. index::
   modff
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modff (x: SHORTREAL;
                                 VAR y: SHORTREAL) : SHORTREAL ;
.. index::
   modfl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ modfl (x: LONGREAL; VAR y: LONGREAL) : LONGREAL ;
    
.. index::
   signbit
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbit (r: REAL) : INTEGER ;
.. index::
   signbitf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbitf (s: SHORTREAL) : INTEGER ;
.. index::
   signbitl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ signbitl (l: LONGREAL) : INTEGER ;
    
.. index::
   nextafter
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafter (x, y: REAL) : REAL ;
.. index::
   nextafterf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafterf (x, y: SHORTREAL) : SHORTREAL ;
.. index::
   nextafterl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nextafterl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   nexttoward
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttoward (x, y: REAL) : LONGREAL ;
.. index::
   nexttowardf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttowardf (x, y: SHORTREAL) : LONGREAL ;
.. index::
   nexttowardl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ nexttowardl (x, y: LONGREAL) : LONGREAL ;
    
.. index::
   scalb
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalb (x, n: REAL) : REAL ;
.. index::
   scalbf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbf (x, n: SHORTREAL) : SHORTREAL ;
.. index::
   scalbl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbl (x, n: LONGREAL) : LONGREAL ;
    
.. index::
   scalbln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbln (x: REAL; n: LONGINT) : REAL ;
.. index::
   scalblnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalblnf (x: SHORTREAL; n: LONGINT) : SHORTREAL ;
.. index::
   scalblnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalblnl (x: LONGREAL; n: LONGINT) : LONGREAL ;
    
.. index::
   scalbn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbn (x: REAL; n: INTEGER) : REAL ;
.. index::
   scalbnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbnf (x: SHORTREAL; n: INTEGER) : SHORTREAL ;
.. index::
   scalbnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ scalbnl (x: LONGREAL; n: INTEGER) : LONGREAL ;
    
    (* complex arithmetic intrincic procedure functions *)
    
.. index::
   cabsf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabsf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   cabs
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabs (z: COMPLEX) : REAL ;
.. index::
   cabsl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cabsl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   cargf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cargf (z: SHORTCOMPLEX) : SHORTREAL ;
.. index::
   carg
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carg (z: COMPLEX) : REAL ;
.. index::
   cargl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cargl (z: LONGCOMPLEX) : LONGREAL ;
    
.. index::
   conjf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conjf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   conj
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conj (z: COMPLEX) : COMPLEX ;
.. index::
   conjl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ conjl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cpowerf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpowerf (base: SHORTCOMPLEX;
                                   exp: SHORTREAL) : SHORTCOMPLEX ;
.. index::
   cpower
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpower (base: COMPLEX; exp: REAL) : COMPLEX ;
.. index::
   cpowerl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cpowerl (base: LONGCOMPLEX;
                                   exp: LONGREAL) : LONGCOMPLEX ;
    
.. index::
   csqrtf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrtf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csqrt
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrt (z: COMPLEX) : COMPLEX ;
.. index::
   csqrtl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csqrtl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   cexpf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexpf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cexp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexp (z: COMPLEX) : COMPLEX ;
.. index::
   cexpl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cexpl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   clnf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ clnf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   cln
.. code-block:: modula2
    PROCEDURE __BUILTIN__ cln (z: COMPLEX) : COMPLEX ;
.. index::
   clnl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ clnl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   csinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   csin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csin (z: COMPLEX) : COMPLEX ;
.. index::
   csinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ csinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ccosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ccos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccos (z: COMPLEX) : COMPLEX ;
.. index::
   ccosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   ctanf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   ctan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctan (z: COMPLEX) : COMPLEX ;
.. index::
   ctanl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ ctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carcsinf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsinf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carcsin
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsin (z: COMPLEX) : COMPLEX ;
.. index::
   carcsinl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carcsinl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carccosf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccosf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carccos
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccos (z: COMPLEX) : COMPLEX ;
.. index::
   carccosl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carccosl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
.. index::
   carctanf
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctanf (z: SHORTCOMPLEX) : SHORTCOMPLEX ;
.. index::
   carctan
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctan (z: COMPLEX) : COMPLEX ;
.. index::
   carctanl
.. code-block:: modula2
    PROCEDURE __BUILTIN__ carctanl (z: LONGCOMPLEX) : LONGCOMPLEX ;
    
    (* memory and string intrincic procedure functions *)
    
.. index::
   alloca
.. code-block:: modula2
    PROCEDURE __BUILTIN__ alloca (i: CARDINAL) : ADDRESS ;
.. index::
   memcpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memcpy (dest, src: ADDRESS;
                                  nbytes: CARDINAL) : ADDRESS ;
.. index::
   index
.. code-block:: modula2
    PROCEDURE __BUILTIN__ index (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   rindex
.. code-block:: modula2
    PROCEDURE __BUILTIN__ rindex (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   memcmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memcmp (s1, s2: ADDRESS;
                                  nbytes: CARDINAL) : INTEGER ;
.. index::
   memset
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memset (s: ADDRESS; c: INTEGER;
                                  nbytes: CARDINAL) : ADDRESS ;
.. index::
   memmove
.. code-block:: modula2
    PROCEDURE __BUILTIN__ memmove (s1, s2: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcat
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcat (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncat
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncat (dest, src: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcpy (dest, src: ADDRESS) : ADDRESS ;
.. index::
   strncpy
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncpy (dest, src: ADDRESS;
                                   nbytes: CARDINAL) : ADDRESS ;
.. index::
   strcmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcmp (s1, s2: ADDRESS) : INTEGER ;
.. index::
   strncmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strncmp (s1, s2: ADDRESS;
                                   nbytes: CARDINAL) : INTEGER ;
.. index::
   strlen
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strlen (s: ADDRESS) : INTEGER ;
.. index::
   strstr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strstr (haystack, needle: ADDRESS) : ADDRESS ;
.. index::
   strpbrk
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strpbrk (s, accept: ADDRESS) : ADDRESS ;
.. index::
   strspn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strcspn
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strcspn (s, accept: ADDRESS) : CARDINAL ;
.. index::
   strchr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
.. index::
   strrchr
.. code-block:: modula2
    PROCEDURE __BUILTIN__ strrchr (s: ADDRESS; c: INTEGER) : ADDRESS ;
    
    (*
       longjmp - this GCC builtin restricts the val to always 1.
    *)
    (* do not use these two builtins, as gcc, only really
       anticipates that the Ada front end should use them
       and it only uses them in its runtime exception handling.
       We leave them here in the hope that someday they will
       behave more like their libc counterparts.  *)
    
.. index::
   longjmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ longjmp (env: ADDRESS; val: INTEGER) ;
.. index::
   setjmp
.. code-block:: modula2
    PROCEDURE __BUILTIN__ setjmp (env: ADDRESS) : INTEGER ;
    
    
    (*
       frame_address - returns the address of the frame.
                       The current frame is obtained if level is 0,
                       the next level up if level is 1 etc.
    *)
    
.. index::
   frame_address
.. code-block:: modula2
    PROCEDURE __BUILTIN__ frame_address (level: CARDINAL) : ADDRESS ;
    
    
    (*
       return_address - returns the return address of function.
                        The current function return address is
                        obtained if level is 0,
                        the next level up if level is 1 etc.
    *)
    
.. index::
   return_address
.. code-block:: modula2
    PROCEDURE __BUILTIN__ return_address (level: CARDINAL) : ADDRESS ;
    
    
    (*
       alloca_trace - this is a no-op which is used for internal debugging.
    *)
    
.. index::
   alloca_trace
.. code-block:: modula2
    PROCEDURE alloca_trace (returned: ADDRESS; nBytes: CARDINAL) : ADDRESS ;
    
    
    END Builtins.
