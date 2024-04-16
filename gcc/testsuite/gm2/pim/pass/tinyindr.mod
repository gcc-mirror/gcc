MODULE tinyindr ;

FROM SYSTEM IMPORT WORD, BYTE ;

TYPE
   File = RECORD
             lastWord: WORD ;
             lastByte: BYTE ;
          END ;

PROCEDURE Create (VAR f: File) ;
BEGIN
   WITH f DO
      lastWord := WORD (0) ;
      lastByte := BYTE (0)
   END
END Create ;


VAR
   foo: File ;
BEGIN
   Create (foo)
END tinyindr.
