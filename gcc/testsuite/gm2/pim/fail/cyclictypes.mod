MODULE cyclictypes ;

TYPE
   A = B;
   B = A;

PROCEDURE foo ;
VAR
   bar: A ;
END foo ;


END cyclictypes.
