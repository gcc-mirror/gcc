MODULE tinybadimport ;

   MODULE inner ;
   IMPORT ASCII ;
   VAR
      foo: CHAR ;
   BEGIN
      foo := ASCII.nul ;
   END inner ;

BEGIN
END tinybadimport.
