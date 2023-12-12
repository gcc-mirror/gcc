IMPLEMENTATION MODULE TextUtil ;

IMPORT CharClass, IOConsts ;

(*
   SkipSpaces - skips any spaces.
*)

PROCEDURE SkipSpaces (cid: IOChan.ChanId) ;
VAR
   ch : CHAR ;
   res: IOConsts.ReadResults ;
BEGIN
   WHILE CharAvailable (cid) DO
      IOChan.Look (cid, ch, res) ;
      IF (res = IOConsts.allRight) AND CharClass.IsWhiteSpace (ch)
      THEN
         IOChan.Skip (cid)
      ELSE
         RETURN
      END
   END
END SkipSpaces ;


(* The following procedures do not read past line marks.  *)

PROCEDURE CharAvailable (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult (cid) = IOConsts.notKnown) OR
           (IOChan.ReadResult (cid) = IOConsts.allRight) )
END CharAvailable ;


PROCEDURE EofOrEoln (cid: IOChan.ChanId) : BOOLEAN ;
BEGIN
   RETURN( (IOChan.ReadResult (cid) = IOConsts.endOfLine) OR
           (IOChan.ReadResult (cid) = IOConsts.endOfInput) )
END EofOrEoln ;


END TextUtil.
