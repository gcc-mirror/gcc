MODULE arrayconst ;


PROCEDURE foo ;
TYPE
   array = ARRAY [0..3] OF REAL ;
CONST
   value = array {1.0, 2.0, 3.0, 4.0} ;
VAR
   r: REAL ;
BEGIN
   r := value[1] ;
END foo ;


PROCEDURE bar ;
TYPE
   array = ARRAY [0..3] OF REAL ;
CONST
   value = array {5.0, 6.0, 7.0, 8.0} ;
VAR
   r: REAL ;
BEGIN
   r := value[1] ;
END bar ;


BEGIN
   foo ;
   bar
END arrayconst.
