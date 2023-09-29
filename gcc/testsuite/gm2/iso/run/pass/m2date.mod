MODULE m2date ;

IMPORT SysClock, STextIO, SWholeIO ;
FROM SysClock IMPORT DateTime, GetClock ;
FROM wrapclock IMPORT tzname ;
FROM ASCII IMPORT nul ;


TYPE
   Name = ARRAY [0..3] OF CHAR ;
   DayArray = ARRAY [0..6] OF Name ;
   MonthArray = ARRAY [0..11] OF Name ;

CONST
   Debugging = FALSE ;
   (* 1st January 1970 was a Thursday.  *)
   DayName   = DayArray { "Thu", "Fri", "Sat", "Sun", "Mon", "Tue", "Wed" } ;
   MonthName = MonthArray { "Dec", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov" } ;



PROCEDURE WriteTZ (daylight: CARDINAL) ;
VAR
   tz : ARRAY [0..10] OF CHAR ;
   ptr: POINTER TO CHAR ;
   i  : CARDINAL ;
BEGIN
   ptr := tzname (daylight) ;
   i := 0 ;
   WHILE (i <= HIGH (tz)) AND (ptr^ # nul) DO
      tz[i] := ptr^ ;
      INC (ptr) ;
      INC (i)
   END ;
   IF i < HIGH (tz)
   THEN
      tz[i] := nul
   END ;
   STextIO.WriteString (tz)
END WriteTZ ;


PROCEDURE WriteNum (num: CARDINAL) ;
BEGIN
   IF num < 10
   THEN
      STextIO.WriteString ("0")
   END ;
   SWholeIO.WriteCard (num, 0)
END WriteNum ;


VAR
   dt: DateTime ;
BEGIN
   IF SysClock.CanGetClock ()
   THEN
      GetClock (dt) ;
      IF Debugging
      THEN
         STextIO.WriteString ("success we can get the clock") ; STextIO.WriteLn ;
         STextIO.WriteString (" year     : ") ; SWholeIO.WriteCard (dt.year, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" month    : ") ; SWholeIO.WriteCard (dt.month, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" day      : ") ; SWholeIO.WriteCard (dt.day, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" hour     : ") ; SWholeIO.WriteCard (dt.hour, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" minute   : ") ; SWholeIO.WriteCard (dt.minute, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" second   : ") ; SWholeIO.WriteCard (dt.second, 4) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" fractions: ") ; SWholeIO.WriteCard (dt.fractions, 10) ;
         STextIO.WriteLn ;
         STextIO.WriteString (" zone     : ") ; SWholeIO.WriteCard (dt.zone, 10) ;
         STextIO.WriteLn
      END ;
      STextIO.WriteString (DayName[dt.day MOD 7]) ;
      STextIO.WriteString (" ") ;
      SWholeIO.WriteCard (dt.day, 2) ;
      STextIO.WriteString (" ") ;
      STextIO.WriteString (MonthName[dt.month MOD 12]) ;
      STextIO.WriteString (" ") ;
      WriteNum (dt.hour) ; STextIO.WriteString (":") ;
      WriteNum (dt.minute) ; STextIO.WriteString (":") ;
      WriteNum (dt.second) ; STextIO.WriteString (" ") ;
      IF dt.summerTimeFlag
      THEN
         WriteTZ (1)
      ELSE
         WriteTZ (0)
      END ;
      STextIO.WriteString (" ") ;
      SWholeIO.WriteCard (dt.year, 0) ;
      STextIO.WriteLn
   ELSE
      STextIO.WriteString ("unable to get the clock") ;
      STextIO.WriteLn ;
      HALT (1)
   END
END m2date.
