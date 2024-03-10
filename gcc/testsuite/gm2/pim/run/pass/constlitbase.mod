MODULE constlitbase ;

FROM libc IMPORT exit, printf ;

CONST
   BaseAddress = 0ABCDH ;
   One         = 0FH ;
   Two         = 0FFH ;
   Three       = 0FFFH ;
   Four        = 0FFFFH ;
   Limit       = 01000 ;
   Oct         = 0100B ;
   Bin         = 0101A ;
   Hex         = 0101H ;
   HexTest     = 01AH ;
   ByteMax     = 011111111A ;


PROCEDURE Assert (var, const: CARDINAL) ;
BEGIN
   IF var # const
   THEN
      printf ("test failed %d # %d\n", var, const) ;
      code := 1
   END
END Assert ;

VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   Assert (BaseAddress, 43981) ;
   Assert (One, 15) ;
   Assert (Two, 255) ;
   Assert (Three, 4095) ;
   Assert (Four, 65535) ;
   Assert (Limit, 1000) ;
   Assert (Oct, 64) ;
   Assert (Bin, 5) ;
   Assert (Hex, 257) ;
   Assert (HexTest, 16+10) ;
   Assert (ByteMax, 255) ;
   exit (code)
END constlitbase.
