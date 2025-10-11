MODULE badprocedure2 ;


PROCEDURE foo1 ;
BEGIN
END foo1 ;

   MODULE inner ;

   IMPORT foo1 ;

   PROCEDURE foo ;
   BEGIN
   END foo ;

   BEGIN
      Foo
   END inner ;

BEGIN
END badprocedure2.
