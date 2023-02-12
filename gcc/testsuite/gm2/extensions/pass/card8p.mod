MODULE card8p ; 


FROM libc IMPORT exit, write ;
FROM ASCII IMPORT nul, nl ;
FROM SYSTEM IMPORT ADR,
                   INTEGER8, INTEGER16, INTEGER32, INTEGER64,
                   CARDINAL8, CARDINAL16, CARDINAL32, CARDINAL64,
                   BYTE, WORD16, WORD32, WORD64 ;
FROM M2RTS IMPORT Length ;
FROM NumberIO IMPORT CardToStr ;

   
PROCEDURE Assert (c: BOOLEAN; line: CARDINAL; column: CARDINAL;
                  message: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   IF NOT c
   THEN
      r := write(2, ADR(__FILE__), Length(__FILE__)) ;
      r := write(2, ADR(": "), Length(":")) ;
      CardToStr(line, 0, a) ;
      r := write(2, ADR(a), Length(a)) ;
      r := write(2, ADR(": "), Length(":")) ;
      CardToStr(column, 0, a) ;
      r := write(2, ADR(a), Length(a)) ;
      r := write(2, ADR(": "), Length(":")) ;
      r := write(2, ADR(message), Length(message)) ;
      a[0] := nl ;
      a[1] := nul ;
      r := write(2, ADR(a), Length(a)) ;
      e := 1
   END
END Assert ;

VAR
   e  : INTEGER ;
   z: (zero, one, two) ;

   c8 : CARDINAL8 ;
   w8 : BYTE ;
   i8 : INTEGER8 ;
   i, j: CARDINAL8;
BEGIN
   e := 0 ;

   i := 0 ;
   INC(i) ;
   Assert(i=1, __LINE__, __COLUMN__, "INC failed to generate value of 1") ;
   i8 := i ;
   Assert(i8=1, __LINE__, __COLUMN__, "assignment failed to propagate via INTEGER8") ;
   c8 := i ;
   Assert(c8=1, __LINE__, __COLUMN__, "assignment failed to propagate via CARDINAL8") ;

   DEC(i) ;
   Assert(i=0, __LINE__, __COLUMN__, "DEC failed to generate value of 0") ;
   j := 1 ;
   INC(i, j) ;
   Assert(i=1, __LINE__, __COLUMN__, "INC failed to generate value of 1") ;
   DEC(i, j) ;
   Assert(i=0, __LINE__, __COLUMN__, "DEC failed to generate value of 0") ;
   INC(i, one) ;
   Assert(i=1, __LINE__, __COLUMN__, "INC failed to generate value of 1") ;
   DEC(i, one) ;
   Assert(i=0, __LINE__, __COLUMN__, "DEC failed to generate value of 0") ;
   i := MAX(CARDINAL8) ;
   j := i ;
   DEC(i) ;
   Assert(i=j-1, __LINE__, __COLUMN__, "DEC failed to generate value of MAX(dataType)-1") ;
   DEC(i, 1) ;
   Assert(i=j-2, __LINE__, __COLUMN__, "DEC failed to generate value of MAX(dataType)-2") ;

   i := 0 ;
   j := 1 ;
   DEC(i) ;
   Assert(i=-1, __LINE__, __COLUMN__, "DEC failed to generate value of -1") ;
   DEC(i, j) ;
   Assert(i=-2, __LINE__, __COLUMN__, "DEC failed to generate value of -2") ;
   INC(i) ;
   Assert(i=-1, __LINE__, __COLUMN__, "DEC failed to generate value of -1") ;
   INC(i, j) ;
   Assert(i=0, __LINE__, __COLUMN__, "DEC failed to generate value of 0") ;
   i := MIN(CARDINAL8) ;
   j := i ;
   INC(i) ;
   Assert(i=j+1, __LINE__, __COLUMN__, "DEC failed to generate value of MIN(dataType)+1") ;
   INC(i, 1) ;
   Assert(i=j+2, __LINE__, __COLUMN__, "DEC failed to generate value of MIN(dataType)+2") ;
   exit(e) ;
END card8p.
