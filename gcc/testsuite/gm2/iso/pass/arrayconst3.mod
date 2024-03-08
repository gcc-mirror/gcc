MODULE arrayconst3 ;


PROCEDURE foo ;
CONST
   len = 4 ;
TYPE
   array = ARRAY [0..len -1] OF REAL ;
CONST
   value = array {1.0, 2.0, 3.0, 4.0} ;
VAR
   r: REAL ;
BEGIN
   r := value[1] ;
END foo ;


PROCEDURE bar ;
CONST
   len = 2 ;
TYPE
   array = ARRAY [0..len -1] OF REAL ;
CONST
   value = array {5.0, 6.0} ;
VAR
   r: REAL ;
BEGIN
   r := value[1] ;
END bar ;


BEGIN
   foo ;
   bar
END arrayconst3.
