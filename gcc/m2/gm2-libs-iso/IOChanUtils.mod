IMPLEMENTATION MODULE IOChanUtils ;

IMPORT IOChan, SFIO, RTio ;


(*
   GetFileName - returns the filename as a new string associated
                 with chanid c.  This string should be killed by
                 the caller.
*)

PROCEDURE GetFileName (c: IOChan.ChanId) : String ;
BEGIN
   RETURN SFIO.GetFileName (RTio.GetFile (c))
END GetFileName ;


END IOChanUtils.
