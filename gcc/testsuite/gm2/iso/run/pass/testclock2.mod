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
         STextIO.WriteString ("we can set the clock, but we won't in the testsuite") ; STextIO.WriteLn
      ELSE
         STextIO.WriteString ("unable to set the clock") ; STextIO.WriteLn
      END
   ELSE
      STextIO.WriteString ("unable to get the clock") ; STextIO.WriteLn
   END
END testclock2.
