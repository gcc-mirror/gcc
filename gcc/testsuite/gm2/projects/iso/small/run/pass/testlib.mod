IMPLEMENTATION MODULE testlib ;


CONST
   Red = cons {2, NIL, arrayT {1, 2, 3}} ;

TYPE
   cons = RECORD
             high: CARDINAL ;
             ptr : opaque ;
             content: arrayT ;
          END ;

   arrayT = ARRAY [MIN(enum)..MAX(enum)] OF CARDINAL ;

   enum = (red, blue, green) ;

   opaque = POINTER TO CHAR ;


END testlib.
