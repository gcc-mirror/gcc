IMPLEMENTATION MODULE IOChanUtils ;

IMPORT IOChan, SFIO, RTio ;


(*
   GetFileName - returns the filename as a new string associated
                 with chanid c.  This string should be killed by
                 the caller.
*)

PROCEDURE GetFileName (c: IOChan.ChanId) : String ;
BEGIN
   RETURN SFIO.GetFileName (GetFile (c))
END GetFileName ;


(*
   GetFile - returns the FIO.File associated with ChanId c.
*)

PROCEDURE GetFile (c: IOChan.ChanId) : File ;
BEGIN
   RETURN RTio.GetFile (c)
END GetFile ;


END IOChanUtils.
