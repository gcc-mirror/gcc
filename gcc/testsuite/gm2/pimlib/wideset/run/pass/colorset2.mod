MODULE colorset2 ;

FROM libc IMPORT printf, exit ;
FROM M2WIDESET IMPORT Equal, Clear, Shift, Rotate ;

TYPE
   color = SET OF (red, green, blue) ;
   set = color ;

CONST
   HighBit = MAX (set) ;


(*
   Assert -
*)

PROCEDURE Assert (bool: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT bool
   THEN
      printf ("%s:%d:assert failed\n", __FILE__, line);
      exit (1)
   END
END Assert ;


(*
   init -
*)

PROCEDURE init ;
VAR
   left, right: set ;
BEGIN
   left := set {} ;
   right := set {green} ;
   Assert (NOT Equal (left, right, HighBit), __LINE__) ;
   Clear (right, HighBit) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;

   left := set {red} ;
   right := set {green} ;
   Assert (NOT Equal (left, right, HighBit), __LINE__) ;
   Shift (right, right, MAX (set), -1) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;
   left := set {red} ;
   right := set {green} ;
   Shift (left, left, MAX (set), 1) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;

   left := set {red} ;
   right := set {green} ;
   Rotate (left, left, MAX (set), 1) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;

   left := set {green} ;
   right := set {red} ;
   Rotate (left, left, MAX (set), -1) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;

   left := set {red} ;
   right := set {blue} ;
   Rotate (left, left, MAX (set), -1) ;
   Assert (Equal (left, right, HighBit), __LINE__) ;
   printf ("All tests pass in %s\n", __FILE__)
END init ;


BEGIN
   init
END colorset2.
