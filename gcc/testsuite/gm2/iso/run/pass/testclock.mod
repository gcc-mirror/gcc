MODULE testclock ;

IMPORT SysClock, STextIO ;

BEGIN
   IF SysClock.CanGetClock ()
   THEN
      STextIO.WriteString ("success we can get the clock") ;
      STextIO.WriteLn
   ELSE
      STextIO.WriteString ("unable to get the clock") ;
      STextIO.WriteLn ;
      HALT (1)
   END
END testclock.
