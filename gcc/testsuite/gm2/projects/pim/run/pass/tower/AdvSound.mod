IMPLEMENTATION MODULE AdvSound ;


FROM AdvMap IMPORT Adjacent ;
IMPORT StrIO ;
FROM AdvSystem IMPORT GetAccessToScreenNo, ReleaseAccessToScreenNo,
                      Player, PlayerNo,
                      NextFreePlayer,
                      IsPlayerActive,
                      GetReadAccessToPlayer,
                      ReleaseReadAccessToPlayer ;


PROCEDURE EnterGame (p: CARDINAL) ;
BEGIN
   GetAccessToScreenNo(p) ;
   StrIO.WriteString('pS start') ; StrIO.WriteLn ;
   ReleaseAccessToScreenNo(p)
END EnterGame ;


(*
   Explode - play the explosion for each player in room, r, and adjacent
             rooms.
*)

PROCEDURE Explode (r: CARDINAL; pulled: CARDINAL; hit: BOOLEAN) ;
VAR
   p: CARDINAL ;
BEGIN
   GetReadAccessToPlayer ;
   FOR p := 0 TO NextFreePlayer-1 DO
      IF IsPlayerActive(p)
      THEN
         WITH Player[p] DO
            IF p=pulled
            THEN
               IF r=RoomOfMan
               THEN
                  GetAccessToScreenNo(p) ;
                  StrIO.WriteString('pS ohnoexplode') ; StrIO.WriteLn ;
                  ReleaseAccessToScreenNo(p)
               ELSIF Adjacent(r, RoomOfMan)
               THEN
                  IF hit
                  THEN
                     GetAccessToScreenNo(p) ;
                     StrIO.WriteString('pS laughexplode') ; StrIO.WriteLn ;
                     ReleaseAccessToScreenNo(p)
                  ELSE
                     GetAccessToScreenNo(p) ;
                     StrIO.WriteString('pS handgrenade') ; StrIO.WriteLn ;
                     ReleaseAccessToScreenNo(p)
                  END
               END
            ELSE
               GetAccessToScreenNo(p) ;
               StrIO.WriteString('pS handgrenade') ; StrIO.WriteLn ;
               ReleaseAccessToScreenNo(p)
            END
         END
      END
   END ;
   ReleaseReadAccessToPlayer
END Explode ;


(*
   ForeachIn - 
*)

PROCEDURE ForeachIn (r: CARDINAL; sound: ARRAY OF CHAR) ;
VAR
   p: CARDINAL ;
BEGIN
   GetReadAccessToPlayer ;
   FOR p := 0 TO NextFreePlayer-1 DO
      IF IsPlayerActive(p)
      THEN
         WITH Player[p] DO
            IF r=RoomOfMan
            THEN
               GetAccessToScreenNo(p) ;
               StrIO.WriteString(sound) ; StrIO.WriteLn ;
               ReleaseAccessToScreenNo(p)
            END
         END
      END
   END ;
   ReleaseReadAccessToPlayer
END ForeachIn ;


(*
   Swish - play the arrow swish sound to each player in room, r.
*)

PROCEDURE Swish (r: CARDINAL) ;
BEGIN
   ForeachIn(r, 'pS arrowswish')
END Swish ;


(*
   Miss - play the arrow miss sound to each player in room, r.
*)

PROCEDURE Miss (r: CARDINAL) ;
BEGIN
   ForeachIn(r, 'pS brokenglass')
END Miss ;


(*
   OhNo - play the OhNo sound to player, p.
*)

PROCEDURE OhNo (p: CARDINAL) ;
BEGIN
   GetAccessToScreenNo(p) ;
   StrIO.WriteString('pS ohno') ; StrIO.WriteLn ;
   ReleaseAccessToScreenNo(p)
END OhNo ;


(*
   Hit - play the arrow hit sound to player, p.
*)

PROCEDURE Hit (p: CARDINAL) ;
BEGIN
   GetAccessToScreenNo(p) ;
   StrIO.WriteString('pS applause') ; StrIO.WriteLn ;
   ReleaseAccessToScreenNo(p)
END Hit ;


END AdvSound.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
