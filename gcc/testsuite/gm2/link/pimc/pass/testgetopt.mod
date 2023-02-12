MODULE testgetopt ;

FROM DynamicStrings IMPORT String, InitString, KillString ;
FROM StringConvert IMPORT stoc ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM ASCII IMPORT nul ;
FROM GetOpt IMPORT GetOpt ;
FROM libc IMPORT printf, exit ;

IMPORT UnixArgs ;


VAR
   MinRoomLength,
   MaxRoomLength,
   MinCorridorLength,
   MaxCorridorLength,
   TotalCorridorLength,
   Seed,
   MaxX,
   MaxY               : INTEGER ;

CONST
   programName = "testgetopt" ;


(*
   help -
*)

PROCEDURE help (code: INTEGER) ;
BEGIN
   printf ("Usage %s [-a minroomsize] [-b maxroomsize] [-c mincorridorlength] [-d maxcorridorlength] [-e totalcorridorlength] [-h] [-o outputfile] [-s seed] [-x maxx] [-y maxy]\n", programName) ;
   printf ("  -a minroomsize            (default is %d)\n", MinRoomLength) ;
   printf ("  -b maxroomsize            (default is %d)\n", MaxRoomLength) ;
   printf ("  -c mincorridorsize        (default is %d)\n", MinCorridorLength) ;
   printf ("  -d maxcorridorsize        (default is %d)\n", MaxCorridorLength) ;
   printf ("  -e totalcorridorlength    (default is %d)\n", TotalCorridorLength) ;
   printf ("  -o outputfile             (default is stdout)\n") ;
   printf ("  -s seed                   (default is %d)\n", Seed) ;
   printf ("  -x minx for whole map     (default is %d)\n", MaxX) ;
   printf ("  -y maxy for whole map     (default is %d)\n", MaxY) ;
   exit (code)
END help ;


(*
   HandleOptions -
*)

PROCEDURE HandleOptions ;
VAR
   optind,
   opterr,
   optopt: INTEGER ;
   arg,
   s, l  : String ;
   ch    : CHAR ;
BEGIN
   l := InitString (':a:b:c:d:e:o:s:hx:y:') ;
   s := NIL ;
   arg := NIL ;
   ch := GetOpt (UnixArgs.GetArgC (), UnixArgs.GetArgV (), l,
                 arg, optind, opterr, optopt) ;
   WHILE ch # nul DO
      CASE ch OF

      'a':  MinRoomLength := stoc (arg) |
      'b':  MaxRoomLength := stoc (arg) |
      'c':  MinCorridorLength := stoc (arg) |
      'd':  MaxCorridorLength := stoc (arg) |
      'e':  TotalCorridorLength := stoc (arg) |
      'h':  help (0) |
      'o':   |
      's':  Seed := stoc (arg) |
      'x':  MaxX := stoc (arg) |
      'y':  MaxY := stoc (arg) |
      '?':  printf ("illegal option\n") ; help (1)

      ELSE
         WriteString ("unrecognised option '-") ; Write (ch) ; WriteString ('"') ; WriteLn ;
         exit (1)
      END ;
      arg := KillString (arg) ;
      ch := GetOpt (UnixArgs.GetArgC (), UnixArgs.GetArgV (), l,
                    arg, optind, opterr, optopt)
   END
END HandleOptions ;


BEGIN
   MinRoomLength := 5 ;
   MaxRoomLength := 10 ;
   MinCorridorLength := 10 ;
   MaxCorridorLength := 15 ;
   TotalCorridorLength := 30 ;
   Seed := 1 ;
   MaxX := 30 ;
   MaxY := 30 ;
   HandleOptions
END testgetopt.