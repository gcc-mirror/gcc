MODULE arrayconst ;


PROCEDURE foo ;
TYPE
   array = ARRAY [0..3] OF REAL ;
CONST
   value = array {1.0, 2.0, 3.0, 4.0} ;
VAR
   c: array ;
BEGIN
   c := value
END foo ;


PROCEDURE bar ;
TYPE
   array = ARRAY [0..3] OF REAL ;
CONST
   value = array {5.0, 6.0, 7.0, 8.0} ;
VAR
   c: array ;
BEGIN
   c := value
END bar ;


BEGIN
   foo ;
   bar
END arrayconst.
