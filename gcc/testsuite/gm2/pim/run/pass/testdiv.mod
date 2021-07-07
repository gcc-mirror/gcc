MODULE testdiv ;  (*!m2pim *)

FROM libc IMPORT printf, exit ;


CONST
   min = -40 ;
   max = 100 ;


(*
   assert -
*)

PROCEDURE assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      printf ("logic failed\n");
      exit (1)
   END
END assert ;


(*
   divtest -
*)

PROCEDURE divtest (a, b: INTEGER) : BOOLEAN ;
BEGIN
   (* firstly catch division by 0.  *)
   IF b = 0
   THEN
      RETURN FALSE
   END ;
   IF max < 0
   THEN
      (* case 2 range is always negative.  *)
      (* in which case a division will be illegal as result will be positive.  *)
      RETURN FALSE
   ELSIF min >= 0
   THEN
      (* case 1 both min / max are positive.  *)
      IF a < b * min
      THEN
         (* underflow.  *)
         RETURN FALSE
      END ;
      IF b > a DIV min
      THEN
         (* underflow.  *)
         RETURN FALSE
      END
   ELSE
      (* case 3 mixed range.  *)
      IF (a >= 0) AND (b > 0)
      THEN
         (* both operands positive therefore cannot overflow.  *)
         RETURN TRUE
      ELSIF (a < 0) AND (b < 0)
      THEN
         (* both operands negative, check for overflow.  *)
         RETURN b < a DIV min
      ELSE
         (* mixed range of operands - only need to test underflow.  *)
         IF a < 0
         THEN
            assert (b >= 0) ;
            (* can underflow if.  *)
            RETURN b > a DIV max
         ELSE
            assert (a >= 0) ;
            assert (b < 0) ;
            (* b is < 0 and the result can underflow if.  *)
            (* printf ("a = %d, b = %d, b * min = %d\n", a, b, b * min);  *)
            RETURN a DIV b >= min
         END
      END
   END ;
   HALT
END divtest ;


(*
   assertFailed -
*)

PROCEDURE assertFailed (a, b: INTEGER; message: ARRAY OF CHAR) ;
BEGIN
   printf ("assert failed ") ;
   printf (message) ;
   printf (" %d DIV %d = %d\n", a, b, a DIV b)
END assertFailed ;


(*
   inRange -
*)

PROCEDURE inRange (v: INTEGER) : BOOLEAN ;
BEGIN
   RETURN (v >= min) AND (v <= max)
END inRange ;


PROCEDURE runTest ;
VAR
   a, b: INTEGER ;
BEGIN
   FOR a := min TO max DO
      FOR b := min TO max DO
         IF b # 0
         THEN
            IF divtest (a, b)
            THEN
               (* printf ("a = %d, b = %d, a DIV b = %d\n", a, b, a DIV b); *)
               printf ("a = %d, b = %d\n", a, b);
               IF NOT inRange (a DIV b)
               THEN
                  assertFailed (a, b, "divtest marked this as good") ;
               END
            ELSE
               IF inRange (a DIV b)
               THEN
                  assertFailed (a, b, "divtest marked this as bad")
               END
            END
         END
      END
   END
END runTest ;


BEGIN
   runTest
END testdiv.
