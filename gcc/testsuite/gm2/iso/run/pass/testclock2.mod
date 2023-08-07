MODULE testclock2 ;

IMPORT SysClock, STextIO ;

VAR
   dt: SysClock.DateTime ;
BEGIN
   IF SysClock.CanGetClock ()
   THEN
      SysClock.GetClock (dt) ;
      IF SysClock.CanSetClock ()
      THEN
         STextIO.WriteString ("success we can set the clock") ; STextIO.WriteLn ;
         SysClock.SetClock (dt)
      ELSE
         STextIO.WriteString ("unable to set the clock") ; STextIO.WriteLn
      END
      ; SysClock.SetClock (dt)
   ELSE
      STextIO.WriteString ("unable to get the clock") ; STextIO.WriteLn
   END
END testclock2.
