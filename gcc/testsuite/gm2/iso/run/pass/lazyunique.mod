MODULE lazyunique ;  (*!m2iso+gm2*)

FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf, exit ;

TYPE
   List = POINTER TO RECORD
                        next : List ;
                        value: INTEGER ;
                     END ;

   Array = ARRAY [0..3] OF INTEGER ;

CONST
   Unsorted = Array {0, 2, 1, 1} ;

VAR
   head: List ;


PROCEDURE Display ;
VAR
   p: List ;
BEGIN
   p := head^.next ;
   printf ("\nunique data\n");
   printf ("===========\n");   
   WHILE p # NIL DO
      printf ("%d\n", p^.value);
      p := p^.next
   END
END Display ;


PROCEDURE Add (VAR p: List; val: INTEGER) ;
BEGIN
   NEW (p) ;
   WITH p^ DO
      value := val ;
      next := NIL
   END
END Add ;


PROCEDURE Unique (val: INTEGER) ;
VAR
   p: List ;
BEGIN
   printf ("new value %d\n", val);
   p := head ;
   (* The following line may cause an exception accessing next or value.  *)
   WHILE p^.next^.value # val DO
      p := p^.next
   END
EXCEPT
   (* Now fixup.  Determine the source of the exception and retry.  *)
   IF head = NIL
   THEN
      printf ("list was empty, add sentinal\n");
      Add (head, -1) ;
      RETRY  (* Jump back to the begin statement.  *)      
   ELSIF p^.next = NIL
   THEN
      printf ("growing the list\n");
      Add (p^.next, val) ;
      RETRY  (* Jump back to the begin statement.  *)
   ELSE
      printf ("should never reach here!\n");   
   END
END Unique ;


PROCEDURE unique ;
VAR
   i: CARDINAL ;
BEGIN
   FOR i := 0 TO HIGH (Unsorted) DO
      Unique (Unsorted[i])
   END ;
   Display
END unique ;


BEGIN
   head := NIL ;
   unique
END lazyunique.
