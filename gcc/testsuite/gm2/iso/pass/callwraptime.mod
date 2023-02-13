MODULE callwraptime ;

FROM wraptime IMPORT tm, InitTM, GetMonth ;

VAR
   m: tm ;
   month: [1..12] ;
BEGIN
   m := InitTM () ;
   month := GetMonth (m)
END callwraptime.
