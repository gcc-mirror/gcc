MODULE card32p ; 


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

   c16: CARDINAL16 ;
   i16: INTEGER16 ;
   w16: WORD16 ;

   c32: CARDINAL32 ;
   i32: INTEGER32 ;
   w32: WORD32 ;
   i, j: CARDINAL32;
BEGIN
   e := 0 ;

   i := 0 ;
   INC(i) ;
   Assert(i=1, __LINE__, __COLUMN__, "INC failed to generate value of 1") ;
   i8 := i ;
   Assert(i8=1, __LINE__, __COLUMN__, "assignment failed to propagate via INTEGER8") ;
   c8 := i ;
   Assert(c8=1, __LINE__, __COLUMN__, "assignment failed to propagate via CARDINAL8") ;

   i16 := i ;
   Assert(i16=1, __LINE__, __COLUMN__, "assignment failed to propagate via INTEGER16") ;
   c16 := i ;
   Assert(c16=1, __LINE__, __COLUMN__, "assignment failed to propagate via CARDINAL16") ;

   i32 := i ;
   Assert(i32=1, __LINE__, __COLUMN__, "assignment failed to propagate via INTEGER32") ;
   c32 := i ;
   Assert(c32=1, __LINE__, __COLUMN__, "assignment failed to propagate via CARDINAL32") ;

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
   i := MAX(CARDINAL32) ;
   j := i ;
   DEC(i) ;
   Assert(i=j-1, __LINE__, __COLUMN__, "DEC failed to generate value of MAX(dataType)-1") ;
   DEC(i, 1) ;
   Assert(i=j-2, __LINE__, __COLUMN__, "DEC failed to generate value of MAX(dataType)-2") ;

   i32 := 0 ;
   j := 1 ;
   DEC(i32) ;
   Assert(i32=-1, __LINE__, __COLUMN__, "DEC failed to generate value of -1") ;
   DEC(i32, j) ;
   Assert(i32=-2, __LINE__, __COLUMN__, "DEC failed to generate value of -2") ;
   INC(i32) ;
   Assert(i32=-1, __LINE__, __COLUMN__, "DEC failed to generate value of -1") ;
   INC(i32, j) ;
   Assert(i32=0, __LINE__, __COLUMN__, "DEC failed to generate value of 0") ;
   i := MIN(CARDINAL32) ;
   j := i ;
   INC(i) ;
   Assert(i=j+1, __LINE__, __COLUMN__, "DEC failed to generate value of MIN(dataType)+1") ;
   INC(i, 1) ;
   Assert(i=j+2, __LINE__, __COLUMN__, "DEC failed to generate value of MIN(dataType)+2") ;
   exit(e) ;
END card32p.
