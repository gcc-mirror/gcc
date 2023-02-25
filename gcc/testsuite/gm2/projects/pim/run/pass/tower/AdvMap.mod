IMPLEMENTATION MODULE AdvMap ;


(* IncPosition increments the x,y coordinates according  *)
(* the Direction sent.                                   *)

PROCEDURE IncPosition (VAR x, y: CARDINAL ; Dir: CARDINAL) ;
BEGIN
   IF (Dir=0) AND (y>0)
   THEN
      DEC(y)
   ELSIF Dir=3
   THEN
      INC(x)
   ELSIF Dir=2
   THEN
      INC(y)
   ELSIF x>0
   THEN
      DEC(x)
   END
END IncPosition ;



(* Adjacent tests whether two rooms R1 & R2 are adjacent *)
(* Assume that access to map has been granted.           *)

PROCEDURE Adjacent (R1, R2: CARDINAL) : BOOLEAN ;
VAR
   i, r1, r2 : CARDINAL ;
   ok: BOOLEAN ;
BEGIN
   WITH Rooms[R1] DO
      i := NoOfDoors ;
      ok := FALSE ;
      WHILE (i>0) AND (NOT ok) DO
         IF Doors[i].LeadsTo=R2
         THEN
            ok := TRUE
         ELSE
            DEC (i)
         END
      END
   END ;
   RETURN ok
END Adjacent ;


BEGIN
   ActualNoOfRooms := 0
END AdvMap.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
