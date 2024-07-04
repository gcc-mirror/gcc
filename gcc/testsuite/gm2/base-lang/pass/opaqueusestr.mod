IMPLEMENTATION MODULE opaqueusestr ;  (*!m2pim*)

FROM opaquestr IMPORT initString, concat ;


PROCEDURE rtos (r: REAL; TotalWidth, FractionWidth: CARDINAL) : String ;
BEGIN
   RETURN ( NIL )
END rtos ;


PROCEDURE doDecimalPlaces (s: String; n: CARDINAL) : String ;
BEGIN
   RETURN concat (s, initString ())
END doDecimalPlaces ;


PROCEDURE IntegerToString (i: INTEGER) : String ;
VAR
   s: String ;
BEGIN
   s := initString () ;
   RETURN s
END IntegerToString ;


END opaqueusestr.
