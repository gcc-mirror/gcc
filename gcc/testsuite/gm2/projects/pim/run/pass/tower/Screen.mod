IMPLEMENTATION MODULE Screen ;


FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrLen ;
FROM TimerHandler IMPORT GetTicks, TicksPerSecond ;

FROM AdvSystem IMPORT Player, PlayerNo, ClientRead,
                      GetAccessToScreenNo,
                      ReleaseAccessToScreenNo, AssignOutputTo ;

FROM AdvMap IMPORT DoorStatus, FileName, MaxLengthOfFileName ;
FROM DynamicStrings IMPORT CopyOut ;

FROM StdIO IMPORT Write ;
IMPORT NumberIO ;
IMPORT StrIO ;


VAR
   mapname: ARRAY [0..12] OF CHAR ;


PROCEDURE AssignMapName (s: String) ;
BEGIN
   CopyOut(mapname, s)
END AssignMapName ;


PROCEDURE WriteCommand (p: CARDINAL; ch: CHAR) ;
BEGIN
   GetAccessToScreenNo(p) ;
   StrIO.WriteString('dCMD ') ; Write(ch) ;
   Write(' ') ;
   CASE ch OF

   'l'      : StrIO.WriteString( 'Left         ') |
   'r'      : StrIO.WriteString( 'Right        ') |
   'v'      : StrIO.WriteString( 'Vault Turn   ') |
   '0'..'9' : StrIO.WriteString( 'Forward ') ; Write(ch) |
   't'      : StrIO.WriteString( 'Thrust       ') |
   'a'      : StrIO.WriteString( 'Attack       ') |
   'p'      : StrIO.WriteString( 'Parry        ') |
   'o'      : StrIO.WriteString( 'Open Door    ') |
   'c'      : StrIO.WriteString( 'Close Door   ') |
   'e'      : StrIO.WriteString( 'Examine Door ') |
   'f'      : StrIO.WriteString( 'Fire Arrow   ') |
   'm'      : StrIO.WriteString( 'Magic Arrow  ') |
   'g'      : StrIO.WriteString( 'Get Treasure ') |
   'd'      : StrIO.WriteString( 'Drop Treasure') |
   'u'      : StrIO.WriteString( 'Use Treasure ') |
   's'      : StrIO.WriteString( 'Speak        ') |
   'w'      : StrIO.WriteString( 'Watch sayeth ') |
   '|'      : StrIO.WriteString( 'Exit Dungeon ')

   ELSE       StrIO.WriteString( 'No Command   ') 
   END ;
   StrIO.WriteLn ;
   ReleaseAccessToScreenNo(p)
END WriteCommand ;


(*
   Sync - 
*)

PROCEDURE Sync (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('sync') ; StrIO.WriteLn
END Sync ;


(*
   Flush - 
*)

PROCEDURE Flush (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('fl') ; StrIO.WriteLn
END Flush ;


(*
   ClearScreen - 
*)

PROCEDURE ClearScreen (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('clear') ; StrIO.WriteLn
END ClearScreen ;

PROCEDURE WriteName (p: CARDINAL; n: ARRAY OF CHAR) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dN ') ; StrIO.WriteString(n) ; StrIO.WriteLn
END WriteName ;

PROCEDURE WriteRoom (p: CARDINAL; r: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dR ') ; NumberIO.WriteCard(r, 0) ; StrIO.WriteLn
END WriteRoom ;

PROCEDURE WriteWounds (p: CARDINAL; w: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dW ') ; NumberIO.WriteCard(w, 0) ; StrIO.WriteLn
END WriteWounds ;

PROCEDURE WriteFatigue (p: CARDINAL; f: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dF ') ; NumberIO.WriteCard(f, 0) ; StrIO.WriteLn
END WriteFatigue ;

PROCEDURE WriteMagicArrows (p: CARDINAL; m: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dM ') ; NumberIO.WriteCard(m, 0) ; StrIO.WriteLn
END WriteMagicArrows ;

PROCEDURE WriteArrows (p: CARDINAL; a: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dA ') ; NumberIO.WriteCard(a, 0) ; StrIO.WriteLn
END WriteArrows ;

PROCEDURE WriteWeight (p: CARDINAL; w: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dw ') ; NumberIO.WriteCard(w, 0) ; StrIO.WriteLn
END WriteWeight ;

PROCEDURE WriteTime (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dT ') ;
   NumberIO.WriteCard((GetTicks() DIV TicksPerSecond) DIV 60, 0) ;
   Write(':') ;
   NumberIO.WriteCard((GetTicks() DIV TicksPerSecond) MOD 60, 0) ;
   StrIO.WriteLn
END WriteTime ;

PROCEDURE WriteFloor (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dMap ') ; StrIO.WriteString('mapname') ; StrIO.WriteLn
END WriteFloor ;

PROCEDURE InitScreen (p: CARDINAL) ;
BEGIN
   GetAccessToScreenNo(p) ;
   Sync(p) ;
   ClearScreen(p) ;

   WITH Player[p] DO
      WriteName(p, ManName) ;
      WriteFloor(p) ;
      WriteRoom(p, RoomOfMan) ;
      WriteWounds(p, Wounds) ;
      WriteFatigue(p, Fatigue) ;
      WriteMagicArrows(p, NoOfMagic) ;
      WriteArrows(p, NoOfNormal) ;
      WriteWeight(p, Weight) ;
      WriteTime(p)
   END ;
   ReleaseAccessToScreenNo(p)
END InitScreen ;


PROCEDURE WriteCommentLine1 (p: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dC1 ') ; StrIO.WriteString(a) ; StrIO.WriteLn
END WriteCommentLine1 ;


PROCEDURE WriteCommentLine2 (p: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dC2 ') ; StrIO.WriteString(a) ; StrIO.WriteLn
END WriteCommentLine2 ;


PROCEDURE WriteCommentLine3 (p: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dC3 ') ; StrIO.WriteString(a) ; StrIO.WriteLn
END WriteCommentLine3 ;


PROCEDURE DelCommentLine1 (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   WriteCommentLine1(p, '')
END DelCommentLine1 ;


PROCEDURE DelCommentLine2 (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   WriteCommentLine2(p, '')
END DelCommentLine2 ;


PROCEDURE DelCommentLine3 (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   WriteCommentLine3(p, '')
END DelCommentLine3 ;


(*
   Pause - issues a pause message and waits for a character to be pressed.
*)

PROCEDURE Pause (p: CARDINAL) ;
VAR
   ch: CHAR ;
BEGIN
   AssignOutputTo(p) ;
   WriteCommentLine2(p, 'Press any key') ;
   WriteCommentLine3(p, 'to continue') ;
   IF ClientRead(ch)
   THEN
   END
END Pause ;


PROCEDURE Quit (p: CARDINAL) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('quit') ; StrIO.WriteLn
END Quit ;


(*
   PromptString - writes a text message to the client console.
                  without the final 'newline'
*)

PROCEDURE PromptString (p: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
   AssignOutputTo(p) ;
   StrIO.WriteString('dWriteStr ') ;
   StrIO.WriteString(a) ;
   StrIO.WriteLn
END PromptString ;


(*
   WriteString - writes a text message to the client console.
*)

PROCEDURE WriteString (p: CARDINAL; a: ARRAY OF CHAR) ;
VAR
   start,
   needEol: BOOLEAN ;
   i, j, n: CARDINAL ;
BEGIN
   AssignOutputTo(p) ;
   n := StrLen(a) ;
   i := 0 ;
   start := TRUE ;
   needEol := FALSE ;
   WHILE i<n DO
      IF (a[i]='\') AND (a[i+1]='n')
      THEN
         IF start
         THEN
            StrIO.WriteString('dWriteLn ')
         END ;
         StrIO.WriteLn ;
         start := TRUE ;
         needEol := FALSE ;
         INC(i)
      ELSE
         IF start
         THEN
            StrIO.WriteString('dWriteLn ') ;
            start := FALSE
         END ;
         Write(a[i]) ;
         needEol := TRUE
      END ;
      INC(i)
   END ;
   IF needEol
   THEN
      StrIO.WriteLn
   END
END WriteString ;


END Screen.
(*
 * Local variables:
 *  compile-command: "make"
 * End:
 *)
