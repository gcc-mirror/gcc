MODULE arrayconst4 ;


PROCEDURE foo ;
CONST
   len = 4 ;
TYPE
   array = ARRAY [0..len -1] OF REAL ;
CONST
   value = array {1.0, 2.0, 3.0, 4.0} ;
VAR
   c: array ;
BEGIN
   c := value
END foo ;


PROCEDURE bar ;
CONST
   len = 2 ;
TYPE
   array = ARRAY [0..len -1] OF REAL ;
CONST
   value = array {5.0, 6.0} ;
VAR
   c: array ;
BEGIN
   c := value
END bar ;


BEGIN
   foo ;
   bar
END arrayconst4.
